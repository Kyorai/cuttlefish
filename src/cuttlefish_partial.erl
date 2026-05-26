%% -------------------------------------------------------------------
%%
%% cuttlefish_partial: load and rewrite a reusable mapping block
%%
%% Copyright (c) 2026 Broadcom. All Rights Reserved. The term Broadcom
%% refers to Broadcom Inc. and/or its subsidiaries.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(cuttlefish_partial).

-export([
    load/3,
    rewrite/2,
    bind_translation/3,
    is_partial_filename/1
]).

-type include_opt() :: {prefix, string()}
                     | {app_prefix, string()}
                     | {exclude, [string()]}
                     | {overrides, [{string(), proplists:property()}]}
                     | {disable_with, atom()}.

-type partial_term() ::
        {mapping, string(), string(), [proplists:property()]}
      | {validator, string(), string(), fun((term()) -> boolean())}
      | {partial_translation, string(),
         fun((cuttlefish_conf:conf(), string(), string()) -> term())}.

-type emitted_term() ::
        {mapping, string(), string(), [proplists:property()]}
      | {validator, string(), string(), fun((term()) -> boolean())}
      | {translation, string(),
         fun((cuttlefish_conf:conf()) -> term())}.

-export_type([include_opt/0, partial_term/0]).

-define(MAX_PARTIAL_BYTES, 8388608).

-spec load(atom(), string(), [include_opt()]) ->
    {ok, [emitted_term()]} | cuttlefish_error:error().
load(App, Name, IncludeOpts) when is_atom(App), is_list(Name) ->
    case validate_include_opts(App, Name, IncludeOpts) of
        ok ->
            case ensure_app_loaded(App) of
                ok ->
                    case resolve_path(App, Name) of
                        {ok, Path} -> load_path(App, Name, Path, IncludeOpts);
                        Err -> Err
                    end;
                Err -> Err
            end;
        Err -> Err
    end.

-spec rewrite([partial_term()], [include_opt()]) ->
    {ok, [emitted_term()]} | cuttlefish_error:error().
rewrite(Terms, IncludeOpts) ->
    Prefix = proplists:get_value(prefix, IncludeOpts),
    AppPrefix = proplists:get_value(app_prefix, IncludeOpts),
    Exclude = proplists:get_value(exclude, IncludeOpts, []),
    Overrides = proplists:get_value(overrides, IncludeOpts, []),
    case rewrite_each(Terms, Prefix, AppPrefix, Exclude, Overrides, []) of
        {ok, Rewritten} ->
            case proplists:get_value(disable_with, IncludeOpts) of
                undefined -> {ok, Rewritten};
                Atom -> {ok, Rewritten ++ disable_with_terms(Prefix, AppPrefix, Atom)}
            end;
        Err -> Err
    end.

-spec bind_translation(Fun3, string(), string()) ->
    fun((cuttlefish_conf:conf()) -> term())
        when Fun3 :: fun((cuttlefish_conf:conf(), string(), string()) -> term()).
bind_translation(Fun3, ConfPrefix, AppPrefix) ->
    fun(Conf) -> Fun3(Conf, ConfPrefix, AppPrefix) end.

%% True for paths ending in `.partial`. Useful for the same kind of
%% pre-flight sanity checks `cuttlefish_schema` applies to `.schema`.
-spec is_partial_filename(file:filename_all()) -> boolean().
is_partial_filename(Filename) ->
    filename:extension(Filename) =:= ".partial".

%% ===================================================================
%% Internal: include-opt validation
%% ===================================================================

validate_include_opts(App, Name, Opts) when is_list(Opts) ->
    case lists:foldl(
            fun(Opt, ok) -> validate_one_opt(Opt);
               (_, Err)  -> Err
            end, ok, Opts) of
        ok ->
            check_required(App, Name, Opts);
        Err -> Err
    end;
validate_include_opts(_App, _Name, Other) ->
    {error, {partial_unknown_include_opt, Other}}.

validate_one_opt({prefix, V}) when is_list(V) -> ok;
validate_one_opt({app_prefix, V}) when is_list(V) -> ok;
validate_one_opt({exclude, L}) when is_list(L) ->
    case lists:all(fun(E) -> is_list(E) andalso E =/= [] end, L) of
        true -> ok;
        false -> {error, {partial_unknown_include_opt, {exclude, L}}}
    end;
validate_one_opt({overrides, L}) when is_list(L) ->
    case lists:all(fun({N, Opt}) ->
                           is_list(N) andalso N =/= []
                               andalso is_tuple(Opt) andalso tuple_size(Opt) >= 1;
                      (_) -> false
                   end, L) of
        true -> ok;
        false -> {error, {partial_unknown_include_opt, {overrides, L}}}
    end;
validate_one_opt({disable_with, V}) when is_atom(V), V =/= undefined -> ok;
validate_one_opt(Other) ->
    {error, {partial_unknown_include_opt, Other}}.

check_required(App, Name, Opts) ->
    case proplists:is_defined(prefix, Opts) of
        false -> {error, {partial_missing_prefix, App, Name}};
        true ->
            case proplists:is_defined(app_prefix, Opts) of
                false -> {error, {partial_missing_app_prefix, App, Name}};
                true ->
                    P = proplists:get_value(prefix, Opts),
                    AP = proplists:get_value(app_prefix, Opts),
                    case {P, AP} of
                        {"", _} -> {error, {partial_empty_prefix, App, Name}};
                        {_, ""} -> {error, {partial_empty_prefix, App, Name}};
                        _ -> ok
                    end
            end
    end.

%% ===================================================================
%% Internal: app + path resolution
%% ===================================================================

ensure_app_loaded(App) ->
    case application:load(App) of
        ok -> ok;
        {error, {already_loaded, App}} -> ok;
        {error, Reason} -> {error, {partial_app_not_loadable, App, Reason}}
    end.

resolve_path(App, Name) ->
    case code:priv_dir(App) of
        {error, bad_name} ->
            {error, {partial_app_no_priv_dir, App}};
        PrivDir ->
            {ok, filename:join([PrivDir, "schema", Name ++ ".partial"])}
    end.

%% ===================================================================
%% Internal: file load + parse
%% ===================================================================

load_path(App, Name, Path, IncludeOpts) ->
    case file:read_file(Path) of
        {ok, Bytes} ->
            decode_and_parse(App, Name, Path, Bytes, IncludeOpts);
        {error, enoent} ->
            {error, {partial_file_not_found, App, Name, Path}};
        {error, Reason} ->
            {error, {partial_file_read_error, App, Name, Path, Reason}}
    end.

decode_and_parse(App, Name, Path, Bytes, IncludeOpts) ->
    case byte_size(Bytes) > ?MAX_PARTIAL_BYTES of
        true ->
            {error, {partial_file_too_large,
                     {Path, byte_size(Bytes), ?MAX_PARTIAL_BYTES}}};
        false ->
            case unicode:characters_to_list(Bytes) of
                {incomplete, _, _} ->
                    {error, {partial_file_invalid_unicode, Path}};
                {error, _, _} ->
                    {error, {partial_file_invalid_unicode, Path}};
                Chardata when is_list(Chardata) ->
                    scan_and_parse(App, Name, Chardata, IncludeOpts)
            end
    end.

scan_and_parse(App, Name, Chardata, IncludeOpts) ->
    case erl_scan:string(Chardata) of
        {ok, Tokens, _} ->
            Comments = erl_comment_scan:string(Chardata),
            Forms = split_forms(Tokens),
            case parse_forms(Forms, Comments, []) of
                {ok, Terms} ->
                    case sanitize(App, Name, Terms) of
                        ok ->
                            case validate_names_against_terms(App, Name, Terms, IncludeOpts) of
                                ok -> rewrite(Terms, IncludeOpts);
                                Err -> Err
                            end;
                        Err -> Err
                    end;
                {error, Inner} ->
                    {error, {partial_parse_error, App, Name, Inner}}
            end;
        {error, {Line, erl_scan, _}, _} ->
            {error, {partial_parse_error, App, Name, {erl_scan, Line}}}
    end.

%% Pre-flight: every `exclude` and `overrides` name on the include
%% must match something in the partial. A typo (`{exclude,
%% ["versiosns"]}') would otherwise silently no-op, leaving the
%% author to discover the misconfiguration through a downstream test
%% (or in production).
validate_names_against_terms(App, Name, Terms, IncludeOpts) ->
    MappingConfs = [K || {mapping, K, _, _} <- Terms],
    TranslationApps = [K || {partial_translation, K, _} <- Terms],
    Sections = [first_segment(K) || K <- MappingConfs],
    ExcludeMatchable = MappingConfs ++ TranslationApps ++ Sections,
    Exclude = proplists:get_value(exclude, IncludeOpts, []),
    OverrideNames = [N || {N, _} <- proplists:get_value(overrides, IncludeOpts, [])],
    case unmatched(Exclude, ExcludeMatchable) of
        [] ->
            case unmatched(OverrideNames, MappingConfs) of
                [] -> ok;
                Bad ->
                    {error, {partial_overrides_unmatched, App, Name, Bad}}
            end;
        Bad ->
            {error, {partial_exclude_unmatched, App, Name, Bad}}
    end.

unmatched(Names, Known) ->
    [N || N <- Names, not lists:member(N, Known)].

first_segment(Key) ->
    [Seg | _] = string:split(Key, "."),
    Seg.

split_forms(Tokens) ->
    split_forms(Tokens, [], []).

split_forms([], [], Acc) ->
    lists:reverse(Acc);
split_forms([], Pending, Acc) ->
    %% No trailing dot: keep the tail so `erl_parse` produces a
    %% useful error rather than silently dropping it.
    lists:reverse([lists:reverse(Pending) | Acc]);
split_forms([{dot, _} = Dot | Rest], Pending, Acc) ->
    Form = lists:reverse([Dot | Pending]),
    split_forms(Rest, [], [Form | Acc]);
split_forms([T | Rest], Pending, Acc) ->
    split_forms(Rest, [T | Pending], Acc).

parse_forms([], _Comments, Acc) ->
    {ok, lists:reverse(Acc)};
parse_forms([Tokens | Rest], Comments, Acc) ->
    case erl_parse:parse_exprs(Tokens) of
        {ok, Exprs} ->
            {value, V, _} = erl_eval:exprs(Exprs, []),
            parse_forms(Rest, Comments,
                        [attach_form_comments(V, Tokens, Comments) | Acc]);
        {error, {Line, erl_parse, Reason}} ->
            {error, {erl_parse, Line, format_parse_reason(Reason)}}
    end.

%% For mapping forms, look up comments that sit above the form's first
%% token and merge them into the form's options proplist as `{doc, _}`
%% and `{see, _}`. Mirrors how `cuttlefish_schema:parse_schema/3` uses
%% `comment_parser/1` and `get_see/1` to attach annotations to inline
%% mappings.
attach_form_comments({mapping, Var, Map, Opts}, Tokens, Comments)
        when is_list(Opts) ->
    FormLine = first_token_line(Tokens),
    Above = comments_above(FormLine, Comments),
    case extract_doc_and_see(Above) of
        {[], []} ->
            {mapping, Var, Map, Opts};
        {Doc, See} ->
            {mapping, Var, Map, merge_doc_see(Opts, Doc, See)}
    end;
attach_form_comments(V, _Tokens, _Comments) ->
    V.

first_token_line([Tok | _]) ->
    case erl_scan:location(Tok) of
        {Line, _Col} -> Line;
        Line when is_integer(Line) -> Line
    end.

%% Returns flat text lines from every comment block whose line is
%% strictly less than `FormLine`.
comments_above(FormLine, Comments) ->
    lists:flatmap(
      fun({CLine, _Col, _N, Lines}) when CLine < FormLine -> Lines;
         (_) -> []
      end, Comments).

%% Tiny `@doc` / `@see` parser that mirrors
%% `cuttlefish_schema:comment_parser/1` but only keeps the two
%% annotations a partial cares about.
extract_doc_and_see(Lines) ->
    Stripped = [strip_percent(L) || L <- Lines],
    do_extract(Stripped, none, [], []).

do_extract([], _Mode, DocAcc, SeeAcc) ->
    {lists:reverse(DocAcc), lists:reverse(SeeAcc)};
do_extract(["@doc " ++ Rest | Tail], _Mode, DocAcc, SeeAcc) ->
    do_extract(Tail, doc, [Rest | DocAcc], SeeAcc);
do_extract(["@see " ++ Rest | Tail], _Mode, DocAcc, SeeAcc) ->
    do_extract(Tail, none, DocAcc, [Rest | SeeAcc]);
do_extract(["@" ++ _ | Tail], _Mode, DocAcc, SeeAcc) ->
    do_extract(Tail, none, DocAcc, SeeAcc);
do_extract([Line | Tail], doc, DocAcc, SeeAcc) ->
    do_extract(Tail, doc, [Line | DocAcc], SeeAcc);
do_extract([_Line | Tail], Mode, DocAcc, SeeAcc) ->
    do_extract(Tail, Mode, DocAcc, SeeAcc).

strip_percent([$% | T]) -> strip_percent(T);
strip_percent([$\s | T]) -> strip_percent(T);
strip_percent(L) -> L.

%% Prepend doc/see entries so they take precedence over anything
%% already in the proplist, matching the inline-schema convention
%% in `cuttlefish_schema:parse_schema/3`.
merge_doc_see(Opts, Doc, See) ->
    DocOpt = case Doc of [] -> []; _ -> [{doc, Doc}] end,
    SeeOpt = case See of [] -> []; _ -> [{see, See}] end,
    DocOpt ++ SeeOpt ++ Opts.

format_parse_reason([H | _] = Reason) when is_list(H) ->
    lists:flatten(Reason);
format_parse_reason(Reason) ->
    io_lib:format("~tp", [Reason]).

%% Reject anything we can't safely rewrite. The errors here are the
%% loud half of the design: a typo'd head atom or a plain `translation`
%% inside a partial fails at load with a message naming the right form.
sanitize(_App, _Name, []) ->
    ok;
sanitize(App, Name, [{mapping, _, _, Opts} | Rest]) when is_list(Opts) ->
    sanitize(App, Name, Rest);
sanitize(App, Name, [{validator, _, _, _} | Rest]) ->
    sanitize(App, Name, Rest);
sanitize(App, Name, [{partial_translation, _, _} | Rest]) ->
    sanitize(App, Name, Rest);
sanitize(_App, _Name, [{translation, _, _} | _]) ->
    {error, {partial_unsupported_term, translation}};
sanitize(App, Name, [{include_partial, _, _} | _]) ->
    {error, {partial_include_in_partial, App, Name}};
sanitize(_App, _Name, [Other | _]) when is_tuple(Other), tuple_size(Other) >= 1 ->
    {error, {partial_unsupported_term, element(1, Other)}};
sanitize(_App, _Name, [Other | _]) ->
    {error, {partial_unsupported_term, Other}}.

%% ===================================================================
%% Internal: term rewriting
%% ===================================================================

rewrite_each([], _Prefix, _AppPrefix, _Exclude, _Overrides, Acc) ->
    {ok, lists:reverse(Acc)};
rewrite_each([Term | Rest], Prefix, AppPrefix, Exclude, Overrides, Acc) ->
    case rewrite_term(Term, Prefix, AppPrefix, Exclude, Overrides) of
        skip ->
            rewrite_each(Rest, Prefix, AppPrefix, Exclude, Overrides, Acc);
        {ok, Rewritten} ->
            rewrite_each(Rest, Prefix, AppPrefix, Exclude, Overrides,
                         [Rewritten | Acc]);
        {error, _} = Err -> Err
    end.

rewrite_term({mapping, ConfKey, AppKey, Opts}, Prefix, AppPrefix, Exclude, Overrides) ->
    case excluded(ConfKey, Exclude) of
        true ->
            skip;
        false ->
            NewOpts = apply_overrides(ConfKey,
                                       rewrite_mapping_opts(Opts, Prefix),
                                       Overrides),
            {ok, {mapping,
                  join_dot(Prefix, ConfKey),
                  join_dot(AppPrefix, AppKey),
                  NewOpts}}
    end;
rewrite_term({validator, _, _, _} = V, _Prefix, _AppPrefix, _Exclude, _Overrides) ->
    {ok, V};
rewrite_term({partial_translation, BareKey, Fun}, Prefix, AppPrefix, Exclude, _Overrides) ->
    case excluded(BareKey, Exclude) of
        true ->
            skip;
        false ->
            case erlang:fun_info(Fun, arity) of
                {arity, 3} ->
                    {ok, {translation,
                          join_dot(AppPrefix, BareKey),
                          bind_translation(Fun, Prefix, AppPrefix)}};
                {arity, Other} ->
                    {error, {partial_translation_bad_arity, BareKey, Other}}
            end
    end.

%% For each override targeting this mapping's bare conf key, prepend
%% its option tuple to Opts. Proplist lookup returns the first match,
%% so prepended overrides win over the partial's defaults.
apply_overrides(ConfKey, Opts, Overrides) ->
    ExtraOpts = [Opt || {Name, Opt} <- Overrides, Name =:= ConfKey],
    ExtraOpts ++ Opts.

%% Emit the historical "<prefix> = <atom>" disable shortcut so each
%% consumer doesn't have to spell out the same guard mapping plus
%% translation. The translation accepts only the specified atom and
%% produces an empty proplist; any other value is rejected with the
%% standard `cuttlefish:invalid` message.
disable_with_terms(Prefix, AppPrefix, Atom) ->
    [{mapping, Prefix, AppPrefix,
        [{datatype, {enum, [Atom]}}]},
     {translation, AppPrefix,
        fun(Conf) ->
            %% Guard equality against captured `Atom` (a pattern would
            %% shadow it).
            case cuttlefish:conf_get(Prefix, Conf, undefined) of
                V when V =:= Atom -> [];
                undefined -> cuttlefish:unset();
                _ -> cuttlefish:invalid("Invalid " ++ Prefix)
            end
        end}].

%% `exclude` matches the bare key exactly OR matches the first dotted
%% segment of a mapping's bare conf key. A bare key with no dots is
%% both "exact" and "section" so this collapses to a single rule.
excluded(BareKey, Exclude) ->
    lists:any(fun(E) -> matches_name_or_section(E, BareKey) end, Exclude).

matches_name_or_section(E, E) -> true;
matches_name_or_section(E, BareKey) ->
    lists:prefix(E ++ ".", BareKey).

rewrite_mapping_opts(Opts, Prefix) ->
    [rewrite_one_opt(Opt, Prefix) || Opt <- Opts].

rewrite_one_opt({see, List}, Prefix) when is_list(List) ->
    {see, [rewrite_see_entry(E, Prefix) || E <- List]};
rewrite_one_opt(Other, _Prefix) ->
    Other.

%% Bare names get the prefix prepended; dotted entries pass through
%% as absolute references. Either way, the result is tokenized so
%% that downstream `cuttlefish_mapping:see/1` returns the same
%% `[variable()]` shape it returns for `@see` comment annotations.
rewrite_see_entry(Entry, Prefix) when is_list(Entry) ->
    Absolute = case lists:member($., Entry) of
                   true -> Entry;
                   false -> join_dot(Prefix, Entry)
               end,
    cuttlefish_variable:tokenize(Absolute);
rewrite_see_entry(Other, _Prefix) ->
    Other.

join_dot([], Tail) -> Tail;
join_dot(Prefix, Tail) -> Prefix ++ "." ++ Tail.
