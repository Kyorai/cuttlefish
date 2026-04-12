%% -------------------------------------------------------------------
%%
%% cuttlefish_generator: this is where the action is
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
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
-module(cuttlefish_generator).

-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(LSUB, "$(").
-define(RSUB, ")").
-define(LSUBLEN, 2).
-define(RSUBLEN, 1).

-export([map/2, map/3, find_mapping/2, add_defaults/2, minimal_map/2, resolve_aliases/2]).

-spec map(cuttlefish_schema:schema(), cuttlefish_conf:conf()) ->
                 [proplists:property()] |
                 {error, atom(), cuttlefish_error:errorlist()}.
map(Schema, Config) ->
    map(Schema, Config, []).

-spec map(cuttlefish_schema:schema(), cuttlefish_conf:conf(), [proplists:property()]) ->
                 [proplists:property()] |
                 {error, atom(), cuttlefish_error:errorlist()}.
map(Schema, Config, ParsedArgs) ->
    map_add_defaults(Schema, Config, ParsedArgs).

%% @doc Generates an Erlang config that only includes the settings
%% encompassed by the passed Config, excluding defaults from the
%% schema for unspecified settings.
-spec minimal_map(cuttlefish_schema:schema(), cuttlefish_conf:conf()) ->
                         [proplists:property()] | {error, atom(), cuttlefish_error:errorlist()}.
minimal_map({AllTranslations,AllMappings,V}, Config) ->
    ConfigKeys = sets:from_list([K || {K, _} <- Config]),
    {RestrictedMappings, MappingKeys} = lists:foldr(fun(M,Acc) ->
                                                            restrict_mappings(M, Acc, ConfigKeys)
                                                    end, {[], sets:new()}, AllMappings),
    RestrictedTranslations = [ T || T <- AllTranslations,
                                    sets:is_element(cuttlefish_translation:mapping(T), MappingKeys)],
    map({RestrictedTranslations,RestrictedMappings,V}, Config, []).

restrict_mappings(M, {Mappings, Keys}, ConfigKeys) ->
    Variable = cuttlefish_mapping:variable(M),
    Aliases = cuttlefish_mapping:aliases(M),
    HasMatch = sets:is_element(Variable, ConfigKeys) orelse
               lists:any(fun(A) -> sets:is_element(A, ConfigKeys) end, Aliases),
    case HasMatch of
        true ->
            {[M|Mappings], sets:add_element(cuttlefish_mapping:mapping(M), Keys)};
        false ->
            {Mappings, Keys}
    end.

-spec map_add_defaults(cuttlefish_schema:schema(), cuttlefish_conf:conf(), [proplists:property()]) ->
                              [proplists:property()] |
                              {error, atom(), cuttlefish_error:errorlist()}.
map_add_defaults({_, Mappings, _} = Schema, Config, ParsedArgs) ->
    %% Config at this point is just what's in the .conf file.
    %% First resolve any aliases to their canonical keys
    _ = ?LOG_DEBUG("Resolving aliases"),
    AliasResolvedConfig = resolve_aliases(Config, Mappings),
    %% add_defaults/2 rolls the default values in from the schema
    _ = ?LOG_DEBUG("Adding Defaults"),
    DConfig = add_defaults(AliasResolvedConfig, Mappings),
    case cuttlefish_error:errorlist_maybe(DConfig) of
        {errorlist, EList} ->
            {error, add_defaults, {errorlist, EList}};
        _ ->
            map_value_sub(Schema, DConfig, ParsedArgs)
    end.

-spec map_value_sub(cuttlefish_schema:schema(), cuttlefish_conf:conf(), [proplists:property()]) ->
                           [proplists:property()] |
                           {error, atom(), cuttlefish_error:errorlist()}.
map_value_sub(Schema, Config, ParsedArgs) ->
    _ = ?LOG_DEBUG("Right Hand Side Substitutions"),
    {_, Mappings, _} = Schema,
     case value_sub(Config) of
         {SubbedConfig, []} ->
            map_transform_datatypes(Schema, SubbedConfig, ParsedArgs);
         {_, EList} ->
            {error, rhs_subs, {errorlist, enrich_alias_sub_errors(EList, Mappings)}}
    end.

-spec map_transform_datatypes(cuttlefish_schema:schema(), cuttlefish_conf:conf(), [proplists:property()]) ->
                                     [proplists:property()] |
                                     {error, atom(), cuttlefish_error:errorlist()}.
map_transform_datatypes({_, Mappings, _} = Schema, DConfig, ParsedArgs) ->
    %% Everything in DConfig is of datatype "string",
    %% transform_datatypes turns them into other erlang terms
    %% based on the schema
    _ = ?LOG_DEBUG("Applying Datatypes"),
    case transform_datatypes(DConfig, Mappings, ParsedArgs) of
        {NewConf, []} ->
            map_validate(Schema, NewConf);
        {_, EList} ->
            {error, transform_datatypes, {errorlist, EList}}
    end.

-spec map_validate(cuttlefish_schema:schema(), cuttlefish_conf:conf()) ->
                          [proplists:property()] |
                          {error, atom(), cuttlefish_error:errorlist()}.
map_validate(Schema, Conf) ->
    %% Any more advanced validators
    _ = ?LOG_DEBUG("Validation"),
    case cuttlefish_error:errorlist_maybe(run_validations(Schema, Conf)) of
        {errorlist, EList} ->
            {error, validation, {errorlist, EList}};
        true ->
            {DirectMappings, TranslationsToDrop} = apply_mappings(Schema, Conf),
            apply_translations(Schema, Conf, DirectMappings, TranslationsToDrop)
    end.

-spec apply_mappings(cuttlefish_schema:schema(), cuttlefish_conf:conf()) ->
                            {[proplists:property()], [string()]}.
apply_mappings({Translations, Mappings, _Validators}, Conf) ->
    %% This fold handles 1:1 mappings, that have no corresponding translations
    %% The accumulator is the app.config proplist that we start building from
    %% these 1:1 mappings, hence the return "DirectMappings".
    %% It also builds a list of "TranslationsToDrop". It's basically saying that
    %% if a user didn't actually configure this setting in the .conf file and
    %% there's no default in the schema, then there won't be enough information
    %% during the translation phase to succeed, so we'll earmark it to be skipped
    {DirectMappings, {TranslationsToMaybeDrop, TranslationsToKeep}} = lists:foldr(
        fun(MappingRecord, {ConfAcc, {MaybeDrop, Keep}}) ->
            Mapping = cuttlefish_mapping:mapping(MappingRecord),
            Default = cuttlefish_mapping:default(MappingRecord),
            Variable = cuttlefish_mapping:variable(MappingRecord),
            case {
                Default =/= undefined orelse cuttlefish_conf:is_variable_defined(Variable, Conf),
                lists:any(
                    fun(T) ->
                        cuttlefish_translation:mapping(T) =:= Mapping
                    end,
                    Translations)
                } of
                {true, false} ->
                    Tokens = cuttlefish_variable:tokenize(Mapping),
                    NewValue = proplists:get_value(Variable, Conf),
                    {set_value(Tokens, ConfAcc, NewValue),
                     {MaybeDrop, ordsets:add_element(Mapping,Keep)}};
                {true, true} ->
                    {ConfAcc, {MaybeDrop, ordsets:add_element(Mapping,Keep)}};
                _ ->
                    {ConfAcc, {ordsets:add_element(Mapping,MaybeDrop), Keep}}
            end
        end,
        {[], {ordsets:new(),ordsets:new()}},
        Mappings),
    _ = ?LOG_DEBUG("Applied 1:1 Mappings"),

    TranslationsToDrop = TranslationsToMaybeDrop -- TranslationsToKeep,
    {DirectMappings, TranslationsToDrop}.

-spec apply_translations(cuttlefish_schema:schema(), cuttlefish_conf:conf(), [proplists:property()], [string()]) ->
                                [proplists:property()] |
                                {error, atom(), cuttlefish_error:errorlist()}.
apply_translations({Translations, _, _} = Schema, Conf, DirectMappings, TranslationsToDrop) ->
    %% The fold handles the translations. After we've build the DirectMappings,
    %% we use that to seed this fold's accumulator. As we go through each translation
    %% we write that to the `app.config` that lives in the accumutator.
    {Proplist, Errorlist} = lists:foldl(fold_apply_translation(Conf, Schema, TranslationsToDrop),
                                        {DirectMappings, []}, Translations),
    case Errorlist of
        [] ->
            _ = ?LOG_DEBUG("Applied Translations"),
            Proplist;
        Es ->
            {error, apply_translations, {errorlist, Es}}
    end.


fold_apply_translation(Conf, Schema, TranslationsToDrop) ->
    fun(TranslationRecord, {Acc, Errors}) ->
            Mapping = cuttlefish_translation:mapping(TranslationRecord),
            Xlat = cuttlefish_translation:func(TranslationRecord),
            case lists:member(Mapping, TranslationsToDrop) of
                false ->
                    {XlatFun, XlatArgs} = prepare_translation_fun(Conf, Schema,
                                                                  Mapping, Xlat),
                    _ = ?LOG_DEBUG("Running translation for ~ts", [Mapping]),
                    case try_apply_translation(Mapping, XlatFun, XlatArgs) of
                        unset ->
                            {Acc, Errors};
                        {set, NewValue} ->
                            {set_value(cuttlefish_variable:tokenize(Mapping), Acc, NewValue), Errors};
                        {error, Term} ->
                            {Acc, [{error, Term}|Errors]}
                    end;
                _ ->
                    _ = ?LOG_DEBUG("~tp in Translations to drop...", [Mapping]),
                    {Acc, Errors}
            end
        end.

prepare_translation_fun(Conf, Schema, Mapping, Xlat) ->
    case proplists:get_value(arity, erlang:fun_info(Xlat)) of
        1 -> {Xlat, [Conf]};
        2 -> {Xlat, [Conf, Schema]};
        OtherArity ->
            {fun(_) ->
                     {error, {translation_arity, {Mapping, OtherArity}}}
             end, error}
    end.

try_apply_translation(Mapping, XlatFun, XlatArgs) ->
    try erlang:apply(XlatFun, XlatArgs) of
        {ok, Value} ->
            {set, Value};
        X ->
            {set, X}
    catch
        %% cuttlefish:conf_get/2 threw not_found
        throw:{not_found, NotFound} ->
            {error, {translation_missing_setting,
                     {Mapping, cuttlefish_variable:format(NotFound)}}};
        %% For explicitly omitting an output setting.
        %% See cuttlefish:unset/0
        throw:unset ->
            unset;
        %% For translations that found invalid
        %% settings, even after mapping. See
        %% cuttlefish:invalid/1.
        throw:{invalid, Invalid} ->
            {error, {translation_invalid_configuration,
                     {Mapping, Invalid}}};
        %% Any unknown error, perhaps caused by stdlib
        %% stuff.
        E:R ->
            {error, {translation_unknown_error,
                     {Mapping, {E, R}}}}
    end.

%for each token, is it special?
%
%if yes, special processing
%if no, recurse into this with the value from the proplist and tail of tokens
%
%unless the tail of tokens is []

%% This is the last token, so things ends with replacing the proplist value.
set_value([LastToken], Acc, NewValue) ->
    cuttlefish_util:replace_proplist_value(list_to_atom(LastToken), NewValue, Acc);
%% This is the case of all but the last token.
%% recurse until you hit a leaf.
set_value([HeadToken|MoreTokens], PList, NewValue) ->
    Token = list_to_atom(HeadToken),
    OldValue = proplists:get_value(Token, PList, []),
    cuttlefish_util:replace_proplist_value(
        Token,
        set_value(MoreTokens, OldValue, NewValue),
        PList).

%% @doc Rewrites substitution_missing_config errors where the missing variable
%% is a known alias key, to produce a more helpful error message.
-spec enrich_alias_sub_errors([cuttlefish_error:error()], [cuttlefish_mapping:mapping()]) ->
    [cuttlefish_error:error()].
enrich_alias_sub_errors(EList, Mappings) ->
    AliasIndex = lists:flatmap(fun(M) ->
        Canonical = cuttlefish_mapping:variable(M),
        [{Alias, Canonical} || Alias <- cuttlefish_mapping:aliases(M)]
    end, Mappings),
    [case E of
        {error, {substitution_missing_config, {Src, Var}}} ->
            VarTokenized = cuttlefish_variable:tokenize(Var),
            case lists:keyfind(VarTokenized, 1, AliasIndex) of
                {_, Canonical} ->
                    {error, {substitution_alias_key,
                             {Src, Var, cuttlefish_variable:format(Canonical)}}};
                false ->
                    E
            end;
        _ -> E
    end || E <- EList].

%% @doc Resolves alias keys to their canonical keys.
%% Alias values are rewritten to the canonical key with a deprecation warning.
%% Canonical takes precedence. If multiple aliases are set, the first one
%% listed in the mapping's `aliases' property wins. Every set alias warns.
-spec resolve_aliases(cuttlefish_conf:conf(), [cuttlefish_mapping:mapping()]) ->
    cuttlefish_conf:conf().
resolve_aliases(Conf, Mappings) ->
    lists:foldl(fun(Mapping, ConfAcc) ->
        case cuttlefish_mapping:aliases(Mapping) of
            [] -> ConfAcc;
            Aliases -> resolve_aliases_for_single_mapping(Mapping, Aliases, ConfAcc)
        end
    end, Conf, Mappings).

resolve_aliases_for_single_mapping(Mapping, Aliases, Conf) ->
    Canonical = cuttlefish_mapping:variable(Mapping),
    CanonicalStr = cuttlefish_variable:format(Canonical),
    lists:foldl(fun(AliasVar, ConfAcc) ->
        %% Recheck on every iteration: a previous alias in this
        %% fold may have inserted the canonical key.
        CanonicalSet = lists:keymember(Canonical, 1, ConfAcc),
        case lists:keyfind(AliasVar, 1, ConfAcc) of
            false ->
                ConfAcc;
            {_, _} when CanonicalSet ->
                AliasStr = cuttlefish_variable:format(AliasVar),
                _ = ?LOG_WARNING("~ts is deprecated and ignored "
                             "in favor of ~ts", [AliasStr, CanonicalStr]),
                lists:keydelete(AliasVar, 1, ConfAcc);
            {_, Value} ->
                AliasStr = cuttlefish_variable:format(AliasVar),
                _ = ?LOG_WARNING("~ts is deprecated, use ~ts instead",
                             [AliasStr, CanonicalStr]),
                [{Canonical, Value} | lists:keydelete(AliasVar, 1, ConfAcc)]
        end
    end, Conf, Aliases).

%% @doc adds default values from the schema when something's not
%% defined in the Conf, to give a complete app.config
add_defaults(Conf, Mappings) ->
    Prefixes = get_possible_values_for_fuzzy_matches(Conf, Mappings),

    lists:foldl(
      fun(MappingRecord, Acc) ->
              case cuttlefish_mapping:has_default(MappingRecord) of
                  false -> Acc;
                  true  -> add_default(Conf, Prefixes, MappingRecord, Acc)
              end
      end,
      Conf, Mappings).

add_default(Conf, Prefixes, MappingRecord, Acc) ->
    Default = cuttlefish_mapping:default(MappingRecord),
    VariableDef = cuttlefish_mapping:variable(MappingRecord),
    IsFuzzyMatch = cuttlefish_mapping:is_fuzzy_variable(MappingRecord),
    IsStrictMatch = lists:keymember(VariableDef, 1, Conf),

    %% No, then plug in the default
    case {IsStrictMatch, IsFuzzyMatch} of
        %% Strict match means we have the setting already
        {true, false} -> Acc;

        %% If IsStrictMatch =:= false, IsFuzzyMatch =:= true, we've got a setting, but
        %% it's part of a complex data structure.
        {false, true} ->
            add_fuzzy_default(Prefixes, Acc, Default, VariableDef);

        %% If Match =:= FuzzyMatch =:= false, use the default, key not set in .conf
        {false, false} -> [{VariableDef, Default}|Acc];

        %% If Match =:= true, do nothing, the value is set in the .conf file
        _ ->
            %% TODO: Handle with more style and grace
            _ = ?LOG_ERROR("Both fuzzy and strict match! should not happen"),
            [{error, {map_multiple_match, VariableDef}}|Acc]
    end.

is_strict_prefix([H|T1], [H|T2]) ->
    is_strict_prefix(T1, T2);
is_strict_prefix([], [H2|_]) when hd(H2) =:= $$ ->
    true;
is_strict_prefix(_, _) ->
    false.

add_fuzzy_default(Prefixes, Conf, Default, VariableDef) ->
    PotentialMatch = lists:dropwhile(fun({Prefix, _}) ->
                                             not is_strict_prefix(Prefix, VariableDef)
                                     end, Prefixes),
    case PotentialMatch of
        %% None of the prefixes match, so we don't generate a default.
        [] -> Conf;
        [{_Prefix, Substitutions}|_] ->
            %% This means that we found the key.
            %% ToAdd will be the list of all the things we're adding to the defaults.
            %% So, let's say you have the following mappings defined:
            %% namespace.$named_thing.a
            %% namespace.$named_thing.b
            %% namespace.$named_thing.c

            %% and in your conf, you defined the following:
            %% namespace.strong_bad.a = 10
            %% namespace.senor_cardgage.b = percent_sign
            %% namespace.trogdor.c = burninate

            %% Well, Prefixes would look like this:
            %% [{"namespace", ["strong_bad", "senor_cardgage", "trogdor"]}]

            %% The ToAdd list comp is going through and saying: ok, I know there are
            %% defaults for namespace.$named_thing.a, b, and c. And I know the possible
            %% values of $named_thing are strong_bad, senor_cardgage, and trogdor.
            %% so I want to ensure that there are values for the following:
            %%
            %% namespace.strong_bad.a
            %% namespace.strong_bad.b
            %% namespace.strong_bad.c
            %% namespace.senor_cardgage.a
            %% namespace.senor_cardgage.b
            %% namespace.senor_cardgage.c
            %% namespace.trogdor.a
            %% namespace.trogdor.b
            %% namespace.trogdor.c

            %% So, we go through the List of possible substitutions
            %% and apply the substitution to the variable. If it
            %% already exists in the Conf, then we skip it, otherwise
            %% we include the Default value.
            ToAdd = [ {VariableToAdd, Default}
                      || Subst <- Substitutions,
                       VariableToAdd <- [cuttlefish_variable:replace_match(VariableDef, Subst)],
                       not lists:keymember(VariableToAdd, 1, Conf)],
            Conf ++ ToAdd
    end.

%%%%%%%%%%%%%%%%%%%%%%%%
%% Prefixes is the thing we need for defaults of named keys
%% it looks like this:
%%
%% Prefixes: [{"riak_control.user",["user"]},
%%     {"listener.https",["internal"]},
%%     {"listener.protobuf",["internal"]},
%%     {"listener.http",["internal"]},
%%     {"multi_backend",
%%      ["bitcask_mult","leveldb_mult","leveldb_mult2","memory_mult"}]
%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_possible_values_for_fuzzy_matches(cuttlefish_conf:conf(), [cuttlefish_mapping:mapping()]) -> [{string(), [string()]}].
get_possible_values_for_fuzzy_matches(Conf, Mappings) ->
    %% Now, get all the variables that could match, i.e. all the names
    %% it found referenced in the Conf proplist. It may look something
    %% like this: [{"n",["ck","ak","bk"]}]
    lists:foldl(
      fun(Mapping, FuzzyMatches) ->
              case cuttlefish_mapping:is_fuzzy_variable(Mapping) of
                  false -> FuzzyMatches; %% Strict match
                  true ->
                      %% Fuzzy match, extract the matching settings from the conf
                      VD = cuttlefish_mapping:variable(Mapping),
                      ListOfVars = [Var || {_, Var} <- cuttlefish_variable:fuzzy_matches(VD, Conf)],
                      {Prefix, _, _} = cuttlefish_variable:split_on_match(VD),
                      orddict:append_list(Prefix, ListOfVars, FuzzyMatches)
              end
      end,
      orddict:new(),
      Mappings).

-spec transform_datatypes(
        cuttlefish_conf:conf(),
        [cuttlefish_mapping:mapping()],
        [proplists:property()]
      ) -> {cuttlefish_conf:conf(), [cuttlefish_error:error()]}.
transform_datatypes(Conf, Mappings, ParsedArgs) ->
    lists:foldl(
        fun({Variable, Value}, {Acc, ErrorAcc}) ->
            AllowExtra = proplists:get_value(allow_extra, ParsedArgs, false),
            %% Look up mapping from schema
            case find_mapping(Variable, Mappings) of
                {error, _} when AllowExtra ->
                    %% user asked for us to tolerate variables
                    %% that are not present in the mapping so
                    %% do nothing here
                    {Acc, ErrorAcc};
                {error, _}  ->
                    %% So, this error message isn't so performant (s/o @argv0)
                    %% but it shouldn't happen too often, and I think it's important
                    %% to give users this feedback.

                    %% It will prevent anything from starting, and will let you know
                    %% that you're trying to set something that has no effect
                    VarName = cuttlefish_variable:format(Variable),
                    _ = ?LOG_ERROR("You've tried to set ~ts, but there is no setting with that name.", [VarName]),
                    _ = ?LOG_ERROR("  Did you mean one of these?"),

                    Possibilities = [ begin
                        MapVarName = cuttlefish_variable:format(cuttlefish_mapping:variable(M)),
                        {cuttlefish_util:levenshtein(VarName, MapVarName), MapVarName}
                    end || M <- Mappings],
                    Sorted = lists:sort(Possibilities),
                    _ = [ _ = ?LOG_ERROR("    ~ts", [T]) || {_, T} <- lists:sublist(Sorted, 3) ],
                    {Acc, [ {error, {unknown_variable, VarName}} | ErrorAcc ]};
                MappingRecord ->
                    DTs = cuttlefish_mapping:datatype(MappingRecord),

                    %% DTs is a list now, which means we'll receive an
                    %% errorlist, not a single error
                    case transform_type(DTs, Value) of
                        {ok, NewValue} ->
                            {[{Variable, NewValue}|Acc], ErrorAcc};
                        {errorlist, EList} ->
                            NewError = {transform_type,
                                        cuttlefish_variable:format(Variable)},
                            {Acc, [{error, NewError}] ++ EList ++ ErrorAcc}
                    end
            end
        end,
        {[], []},
        Conf).

-spec value_sub(cuttlefish_conf:conf()) -> {cuttlefish_conf:conf(), [cuttlefish_error:error()]}.
value_sub(Conf) ->
    lists:foldr(
        fun({Var, Val}, {Acc, ErrorAcc}) ->
            case value_sub(Var, Val, Conf) of
                {error, _E} = Error -> {Acc, [Error|ErrorAcc]};
                {NewVal, _NewConf} -> {[{Var, NewVal}|Acc], ErrorAcc}
            end
        end,
        {[],[]},
        Conf).

-spec value_sub(cuttlefish_variable:variable(),
                string(),
                cuttlefish_conf:conf()) ->
      {string(), cuttlefish_conf:conf()} | cuttlefish_error:error().
value_sub(Var, Value, Conf) ->
    value_sub(Var, Value, Conf, []).

-spec value_sub(cuttlefish_variable:variable(),
                string(),
                cuttlefish_conf:conf(),
                [string()]) ->
      {string(), cuttlefish_conf:conf()} | cuttlefish_error:error().
value_sub(Var, Value, Conf, History) when is_list(Value) ->
     %% Check if history contains duplicates. if so error
     case erlang:length(History) == sets:size(sets:from_list(History)) of
         false ->
             {error, {circular_rhs, History}};
         _ ->
             case head_sub(Value) of
                 none -> {Value, Conf};
                 {sub, NextVar, {SubFront, SubBack}} ->
                    case subbed_value(NextVar, Conf) of
                        undefined ->
                            {error, {substitution_missing_config,
                                     {cuttlefish_variable:format(Var),
                                      cuttlefish_variable:format(NextVar)}}};
                        SubVal ->
                            %% Do a sub-subsitution, in case the substituted
                            %% value contains substitutions itself. Do this as
                            %% its own separate recursion so that circular
                            %% substitutions can be detected.
                            case value_sub(NextVar, SubVal, Conf, [Var|History]) of
                                {error, _} = Error ->
                                    Error;
                                {NewSubVal, NewConf} ->
                                    NewValue = SubFront ++ NewSubVal ++ SubBack,
                                    value_sub(Var, NewValue, NewConf, History)
                            end
                    end
             end
     end;
value_sub(_Var, Value, Conf, _History) ->
    {Value, Conf}.

-spec subbed_value(Var :: cuttlefish_variable:variable(),
                   Conf :: cuttlefish_conf:conf()) -> undefined | string().
subbed_value(Var, Conf) ->
    case proplists:get_value(Var, Conf) of
        undefined ->
            % we couldn't find the var in the conf file, let's
            % look at the environment and check for it
            case os:getenv(cuttlefish_variable:format(Var)) of
                false ->
                    undefined;
                Value ->
                    Value
            end;
        Value ->
            Value
    end.

-spec head_sub(string()) -> none | {sub, cuttlefish_variable:variable(), {string(), string()}}.
head_sub(Value) ->
    L = string:str(Value, ?LSUB),
    case L > 0 of
        false ->
            none;
        _ ->
            R = string:str(string:substr(Value, L + ?RSUBLEN), ?RSUB) + L,
            case L < R of
                false ->
                    none;
                _ ->
                    Var = cuttlefish_variable:tokenize(string:strip(string:substr(Value, L+?LSUBLEN, R-L-?LSUBLEN))),
                    Front = string:substr(Value, 1, L-1),
                    Back =  string:substr(Value, R+?RSUBLEN),
                    {sub, Var, {Front, Back}}
            end
    end.

%% If transform_type takes a list as first argument, foldm_either will
%% give us back an errorlist for a single error
-spec transform_type(cuttlefish_datatypes:datatype_list() | cuttlefish_datatypes:datatype(), term()) ->
                            {ok, term()} | cuttlefish_error:error() | cuttlefish_error:errorlist().
transform_type(DTs, Value) when is_list(DTs) ->
    foldm_either(fun(DT) -> transform_type(DT, Value) end, DTs);

transform_type(DT, Value) ->
    Supported = cuttlefish_datatypes:is_supported(DT),
    Extended = cuttlefish_datatypes:is_extended(DT),
    if
        Supported -> transform_supported_type(DT, Value);
        Extended  -> transform_extended_type(DT, Value);
        true ->
            {error, {unsupported_type, DT}}
    end.

-spec transform_supported_type(cuttlefish_datatypes:datatype(), any()) ->
                                     {ok, term()} | cuttlefish_error:error().
transform_supported_type(DT, Value) ->
    try cuttlefish_datatypes:from_string(Value, DT) of
        {error, Message} -> {error, Message};
        NewValue -> {ok, NewValue}
    catch
        Class:Error:_Stacktrace ->
            %% io:format("Failed to transform a type. Stacktrace: ~p~n", [Stacktrace]),
            {error, {transform_type_exception, {DT, {Class, Error}}}}
    end.

-spec transform_extended_type(cuttlefish_datatypes:extended(), any()) ->
                                     {ok, term()} | cuttlefish_error:error().
transform_extended_type({DT, AcceptableValue}, Value) ->
    case transform_supported_type(DT, Value) of
        {ok, AcceptableValue} ->
            {ok, AcceptableValue};
        {ok, _NewValue} ->
            {error, {transform_type_unacceptable, {Value, AcceptableValue}}};
        {error, Term} ->
            {error, Term}
    end.
%% Ok, this is tricky
%% There are three scenarios we have to deal with:
%% 1. The mapping is there! -> return mapping
%% 2. The mapping is not there -> error
%% 3. The mapping is there, but the key in the schema contains a $.
%%      (fuzzy match)
find_mapping([H|_]=Variable, Mappings) when is_list(H) ->
    {HardMappings, FuzzyMappings} =  lists:foldl(
        fun(Mapping, {HM, FM}) ->
            VariableDef = cuttlefish_mapping:variable(Mapping),
            case {Variable =:= VariableDef, cuttlefish_variable:is_fuzzy_match(Variable, VariableDef)} of
                {true, _} -> {[Mapping|HM], FM};
                {_, true} -> {HM, [Mapping|FM]};
                _ -> {HM, FM}
            end
        end,
        {[], []},
        Mappings),

    %% The input to this function is massaged enough that you'll never see a hard mapping count > 1
    %% You might see more than one fuzzy match, there's really nothing to stop that.
    FVariable = cuttlefish_variable:format(Variable),
    case {length(HardMappings), length(FuzzyMappings)} of
        {1, _} -> hd(HardMappings);
        {0, 1} -> hd(FuzzyMappings);
        {0, 0} -> {error, {mapping_not_found, FVariable}};
        {X, Y} -> {error, {mapping_multiple, {FVariable, {X, Y}}}}
    end;
find_mapping(Variable, Mappings) ->
    find_mapping(cuttlefish_variable:tokenize(Variable), Mappings).

-spec run_validations(cuttlefish_schema:schema(), cuttlefish_conf:conf())
    -> boolean()|list(cuttlefish_error:error()).
run_validations({_, Mappings, Validators}, Conf) ->
    Validations = lists:flatten([ begin
        Vs = cuttlefish_mapping:validators(M, Validators),
        Value = proplists:get_value(cuttlefish_mapping:variable(M), Conf),
        [ begin
            Validator = cuttlefish_validator:func(V),
            case {Value, Validator(Value)} of
                {undefined, _} -> true;
                {_, true} ->
                    true;
                _ ->
                    Error = {validation, { cuttlefish_variable:format(
                                             cuttlefish_mapping:variable(M)),
                                           cuttlefish_validator:description(V)
                                         }},
                    _ = ?LOG_ERROR(cuttlefish_error:xlate(Error)),
                    {error, Error}
            end
        end || V <- Vs]

     end || M <- Mappings,
            cuttlefish_mapping:validators(M) =/= [],
            cuttlefish_mapping:default(M) =/= undefined orelse proplists:is_defined(cuttlefish_mapping:variable(M), Conf)
            ]),
    case lists:all(fun(X) -> X =:= true end, Validations) of
        true -> true;
        _ -> Validations
    end.


%% @doc Calls Fun on each element of the list until it returns {ok,
%% term()}, otherwise accumulates {error, term()} into a list,
%% wrapping in {error, _} at the end.
-spec foldm_either(fun((term()) ->
                           {ok, term()}  | cuttlefish_error:errorlist()),
                   list()) ->
                      {ok, term()}  | cuttlefish_error:errorlist().
foldm_either(Fun, List) ->
    foldm_either(Fun, List, []).

%% @doc Calls Fun on each element of the list until it returns {ok,
%% term()}, otherwise accumulates {error, term()} into a list,
%% wrapping in {errorlist, _} at the end.
-spec foldm_either(fun((term()) ->
                           {ok, term()}  | cuttlefish_error:error()),
                   list(), list()) ->
                      {ok, term()}  | cuttlefish_error:errorlist().
foldm_either(_Fun, [], Acc) -> {errorlist, lists:reverse(Acc)};
foldm_either(Fun, [H|T], Acc) ->
    case Fun(H) of
        {ok, Result} -> {ok, Result};
        {error, _}=Error ->
            foldm_either(Fun, T, [Error|Acc])
    end.

-ifdef(TEST).

-define(XLATE(X), lists:flatten(cuttlefish_error:xlate(X))).

bad_conf_test() ->
    Conf = [
        {["integer_thing"], "thirty_two"},
        {["enum_thing"], bad_enum_value},
        {["ip_thing"], "not an IP address"}
    ],

    Mappings = [
        cuttlefish_mapping:parse({mapping, "integer_thing", "to.int", [
            {datatype, integer}
        ]}),
        cuttlefish_mapping:parse({mapping, "enum_thing", "to.enum", [
            {datatype, {enum, [on, off]}}
        ]}),
        cuttlefish_mapping:parse({mapping, "ip_thing", "to.ip", [
            {datatype, ip}
        ]})
    ],

    Translations = [
        cuttlefish_translation:parse({translation, "to.enum", fun(_ConfConf) -> whatev end})
    ],

    NewConfig = map({Translations, Mappings, []}, Conf),
    io:format("NewConf: ~tp~n", [NewConfig]),

    ?assertMatch({error, transform_datatypes, _}, NewConfig),
    ok.

add_defaults_test() ->
    Conf = [
        %%{["a","b","c"], "override"}, %% Specifically left out. Uncomment line to break test,
        {["a","c","d"], "override"},
        {["no","match"], "unchanged"},
        %%{"m.rk.x", "defined"}, %% since this is undefined no defaults should be created for "m",

        %% two matches on a name "ak" and "bk"
        {["n","ak","x"], "set_n_name_x"},
        {["n","bk","x"], "set_n_name_x2"},
        {["n","ck","y"], "set_n_name_y3"}
    ],

    Mappings = [
        %% First mapping, direct, not in .conf, will be default
        cuttlefish_mapping:parse({mapping, "a.b.c", "b.c", [
                {default, "q"}
            ]}),
        %% default is "l", but since "a.c.d" is in Conf, it will be "override"
        cuttlefish_mapping:parse({mapping, "a.c.d", "c.d", [
                {default, "l"}
            ]}),
        cuttlefish_mapping:parse({mapping, "m.$name.x", "some.proplist", [
                {default, "m_name_x"}
            ]}),
        cuttlefish_mapping:parse({mapping, "n.$name.x", "some.proplist", [
            {default, "n_name_x"}
        ]}),
        cuttlefish_mapping:parse({mapping, "n.$name.y", "some.proplist", [
            {default, "n_name_y"}
        ]}),
        cuttlefish_mapping:parse({mapping, "o.$name.z", "some.proplist", [
            {default, "o_name_z"},
            {include_default, "blue"}
        ]})
    ],

    DConf = add_defaults(Conf, Mappings),
    io:format("DConf: ~tp~n", [DConf]),
    ?assertEqual(9, length(DConf)),
    ?assertEqual("q",               proplists:get_value(["a","b","c"], DConf)),
    ?assertNotEqual("l",            proplists:get_value(["a","c","d"], DConf)),
    ?assertEqual("override",        proplists:get_value(["a","c","d"], DConf)),
    ?assertEqual("unchanged",       proplists:get_value(["no","match"], DConf)),
    ?assertEqual("set_n_name_x",    proplists:get_value(["n","ak","x"], DConf)),
    ?assertEqual("set_n_name_x2",   proplists:get_value(["n","bk","x"], DConf)),
    ?assertEqual("n_name_x",        proplists:get_value(["n","ck","x"], DConf)),
    ?assertEqual("n_name_y",        proplists:get_value(["n","ak","y"], DConf)),
    ?assertEqual("n_name_y",        proplists:get_value(["n","bk","y"], DConf)),
    ?assertEqual("set_n_name_y3",   proplists:get_value(["n","ck","y"], DConf)),
    ?assertEqual(undefined,         proplists:get_value(["o","blue","z"], DConf)),
    ok.

map_test() ->
    Schema = cuttlefish_schema:file("test/riak.schema"),

    Conf = conf_parse:file("test/riak.conf"),

    NewConfig = map(Schema, Conf),

    NewRingSize = proplists:get_value(ring_creation_size, proplists:get_value(riak_core, NewConfig)),
    ?assertEqual(32, NewRingSize),

    NewAAE = proplists:get_value(anti_entropy, proplists:get_value(riak_kv, NewConfig)),
    ?assertEqual({on,[debug]}, NewAAE),

    NewSASL = proplists:get_value(sasl_error_logger, proplists:get_value(sasl, NewConfig)),
    ?assertEqual(false, NewSASL),

    NewHTTP = proplists:get_value(http, proplists:get_value(riak_core, NewConfig)),
    ?assertEqual([{"10.0.0.1", 80}, {"127.0.0.1", 8098}], NewHTTP),

    NewPB = proplists:get_value(pb, proplists:get_value(riak_api, NewConfig)),
    ?assertEqual([], NewPB),

    NewHTTPS = proplists:get_value(https, proplists:get_value(riak_core, NewConfig)),
    ?assertEqual(undefined, NewHTTPS),
    ok.

minimal_map_test() ->
    Schema = cuttlefish_schema:file("test/riak.schema"),
    Conf = [{["ring_size"], "32"},
            {["anti_entropy"], "debug"}],
    NewConfig = minimal_map(Schema, Conf),
    ?assertEqual([{riak_core, [{ring_creation_size, 32}]},{riak_kv,[{anti_entropy, {on, [debug]}}]}],
                 lists:sort(NewConfig)).


apply_mappings_test() ->
    %% Two mappings, both alike in dignity,
    %% In fair unit test, where we lay our scene,
    %% From ancient failure break to new mutiny,
    %% Where civil overrides makes civil priority unclean.
    %% From forth the fatal loins of these two foes
    %% A pair of star-cross'd mappings write one app var;
    %% Whose misadventured piteous overthrows
    %% Do with their merge behave unexpectedly.

    %% Assume add_defaults has already run
    Conf = [
        {["conf", "key1"], "1"},
        {["conf", "key2"], "2"}
    ],
    Mappings = [
        cuttlefish_mapping:parse({
            mapping,
            "conf.key1",
            "erlang.key",
            [
                {default, "1"}
            ]
        }),
        cuttlefish_mapping:parse({
            mapping,
            "conf.key2",
            "erlang.key",
            [
                {default, "2"}
            ]
        })
    ],

    {DirectMappings, []} = apply_mappings({[], Mappings, []}, Conf),
    cuttlefish_unit:assert_config(DirectMappings, "erlang.key", "1"),
    ok.

find_mapping_test() ->
    Mappings = [
        cuttlefish_mapping:parse({mapping, "variable.with.fixed.name", "", [{ default, 0}]}),
        cuttlefish_mapping:parse({mapping, "variable.with.$matched.name", "",  [{ default, 1}]})
    ],
    io:format("Mappings: ~tp~n", [Mappings]),

    ?assertEqual(
        ["variable","with","fixed","name"],
        cuttlefish_mapping:variable(find_mapping(["variable","with","fixed","name"], Mappings))
        ),

    ?assertEqual(
        ["variable","with","fixed","name"],
        cuttlefish_mapping:variable(find_mapping("variable.with.fixed.name", Mappings))
        ),

    ?assertEqual(
        0,
        cuttlefish_mapping:default(find_mapping(["variable","with","fixed","name"], Mappings))
        ),

    ?assertEqual(
        ["variable","with","$matched","name"],
        cuttlefish_mapping:variable(find_mapping(["variable","with","A","name"], Mappings))
        ),

    ?assertEqual(
        1,
        cuttlefish_mapping:default(find_mapping(["variable","with","A","name"], Mappings))
        ),

    ?assertEqual(
        ["variable","with","$matched","name"],
        cuttlefish_mapping:variable(find_mapping(["variable","with","B","name"], Mappings))
        ),

    ?assertEqual(
        1,
        cuttlefish_mapping:default(find_mapping(["variable","with","B","name"], Mappings))
        ),

    ?assertEqual(
        ["variable","with","$matched","name"],
        cuttlefish_mapping:variable(find_mapping(["variable","with","C","name"], Mappings))
        ),

    ?assertEqual(
        1,
        cuttlefish_mapping:default(find_mapping(["variable","with","C","name"], Mappings))
        ),

    ?assertEqual(
        ["variable","with","$matched","name"],
        cuttlefish_mapping:variable(find_mapping(["variable","with","D","name"], Mappings))
        ),

    ?assertEqual(
        1,
        cuttlefish_mapping:default(find_mapping(["variable","with","D","name"], Mappings))
        ),

    ?assertEqual(
        ["variable","with","$matched","name"],
        cuttlefish_mapping:variable(find_mapping(["variable","with","E","name"], Mappings))
        ),

    ?assertEqual(
        1,
        cuttlefish_mapping:default(find_mapping(["variable","with","E","name"], Mappings))
        ),

    %% Test variable name with dot
    ?assertEqual(
       "variable.with.E.F.name not_found",
       ?XLATE(find_mapping(["variable","with","E","F","name"], Mappings))
        ),
    %% Test variable name with escaped dot
    ?assertEqual(
        1,
        cuttlefish_mapping:default(find_mapping(["variable","with","E.F","name"], Mappings))
        ),
    ok.

multiple_hard_match_test() ->
    %% In real life this should never happen, but if it does, I'd love a log message
    Mappings = [
        cuttlefish_mapping:parse({mapping, "variable.with.fixed.name", "", [{ default, 0}]}),
        cuttlefish_mapping:parse({mapping, "variable.with.fixed.name", "",  [{ default, 1}]})
    ],
    ?assertEqual(
       "2 hard mappings and 0 fuzzy mappings found for variable.with.fixed.name",
       ?XLATE(find_mapping(["variable","with","fixed","name"], Mappings))
        ),
    ok.

apply_mappings_translations_dropped_correctly_test() ->
    Fun = fun(X) -> X end,
    ?assertEqual(1, Fun(1)), %% coverage kludge

    Translations = [
        cuttlefish_translation:parse({
            translation,
            "mapping.name",
            Fun
            })
    ],
    Mappings = [
        cuttlefish_mapping:parse({
            mapping,
            "conf.key",
            "mapping.name",
            [{default, 6}]
            })
    ],
    %% So, we have a translation for the corresponding mapping, but that mapping has no default
    {_DirectMappings, TranslationsToDrop} = apply_mappings({Translations, Mappings, []}, []),
    ?assertEqual([], TranslationsToDrop),
    ok.

apply_mappings_translations_dropped_correctly_mixed_test() ->
    Fun = fun(X) -> X end,
    ?assertEqual(1, Fun(1)), %% coverage kludge

    Translations = [
        cuttlefish_translation:parse({
            translation,
            "mapping.name",
            Fun
            })
    ],
    Mappings = [
        cuttlefish_mapping:parse({
            mapping,
            "conf.key",
            "mapping.name",
            [{default, 6}]
            }),
        cuttlefish_mapping:parse({
            mapping,
            "conf.key2",
            "mapping.name",
            []
            })
    ],
    %% One valid mapping, and one that should be dropped (no default)
    {_DirectMappings, TranslationsToDrop} = apply_mappings({Translations, Mappings, []}, []),
    ?assertEqual([], TranslationsToDrop),
    ok.
apply_mappings_translations_dropped_correctly_mixed2_test() ->
    Fun = fun(X) -> X end,
    ?assertEqual(1, Fun(1)), %% coverage kludge

    Translations = [
        cuttlefish_translation:parse({
            translation,
            "mapping.name",
            Fun
            })
    ],
    Mappings = [
        cuttlefish_mapping:parse({
            mapping,
            "conf.key",
            "mapping.name",
            [{default, 6}]
            }),
        cuttlefish_mapping:parse({
            mapping,
            "conf.key2",
            "mapping.name",
            []
            }),
        cuttlefish_mapping:parse({
            mapping,
            "conf.key3",
            "mapping.name",
            []
            })
    ],
    %% One valid mapping and two that should be dropped (no default)
    {_DirectMappings, TranslationsToDrop} = apply_mappings({Translations, Mappings, []}, []),
    ?assertEqual([], TranslationsToDrop),
    ok.

transform_datatypes_not_found_test() ->
    Mappings = [
        cuttlefish_mapping:parse({
            mapping,
            "conf.key",
            "erlang.key",
            []
            })
    ],

    Conf = [
        {["conf", "other"], "string"}
    ],
    NewConf = transform_datatypes(Conf, Mappings, []),
    ?assertEqual({[], [{error, {unknown_variable, "conf.other"}}]}, NewConf),
    ok.

transform_datatypes_allowed_not_found_test() ->
    Mappings = [
        cuttlefish_mapping:parse({
            mapping,
            "conf.key",
            "erlang.key",
            []
            })
    ],

    Conf = [
        {["conf", "other"], "string"}
    ],
    NewConf = transform_datatypes(Conf, Mappings, [{allow_extra, true}]),
    ?assertEqual({[], []}, NewConf),
    ok.

validation_test() ->

    Pid = self(),

    Mappings = [cuttlefish_mapping:parse(
        {mapping, "a", "b.c", [{validators, ["a"]}, {datatype, {enum, [true, false]}}]}
        )
    ],

    Validators = [cuttlefish_validator:parse(
        {validator, "a", "error msg", fun(X) -> Pid ! X, true end}
        )
    ],

    Conf = [
        {["a"], true}
    ],

    AppConf = map({[], Mappings, Validators}, Conf),

    receive
        X ->
            ?assert(X)
    after
        1000 ->
            ?assert(false)
    end,

    ?assertEqual([{b, [{c, true}]}], AppConf),
    ok.

throw_unset_test() ->
    Mappings = [cuttlefish_mapping:parse({mapping, "a", "b.c", []})],
    Translations = [cuttlefish_translation:parse(
        {translation, "b.c",
        fun(_X) -> cuttlefish:unset() end})
    ],
    AppConf = map({Translations, Mappings, []}, []),
    ?assertEqual([], AppConf),
    ok.

bad_prefix_match_test() ->
    Prefixes = [
        {["prefix", "one"], ["one", "two"]},
        {["prefix", "two"], ["one", "two"]}
    ],
    Conf = [],
    Default = 8,
    VariableDef = ["prefix", "one", "other_thing", "$name"],

    ?assertEqual([], add_fuzzy_default(Prefixes, Conf, Default, VariableDef)),
    ok.

assert_extended_datatype(
  Datatype,
  Setting,
  Expected) ->
    Mappings = [
        cuttlefish_mapping:parse({mapping, "a.b", "e.k", [
            {datatype, Datatype}
        ]})
    ],
    Conf = [
        {["a","b"], Setting}
    ],

    Actual = map({[], Mappings, []}, Conf),

    case Expected of
        {error, Phase, EMsg} ->
            ?assertMatch({error, Phase, _}, Actual),
            ?assertEqual(EMsg, ?XLATE(hd(element(2, element(3, Actual)))));
        _ ->
            ?assertEqual([{e, [{k, Expected}]}], map({[], Mappings, []}, Conf))
    end,
    ok.

extended_datatypes_test() ->
    assert_extended_datatype([integer, {atom, never}], "1", 1),
    assert_extended_datatype([integer, {atom, never}], "never", never),
    assert_extended_datatype([integer, {atom, never}], "always", {error, transform_datatypes, "Error transforming datatype for: a.b"}),
    assert_extended_datatype([{duration, s}, {atom, never}], "never", never),
    assert_extended_datatype([{atom, never}, integer], "1", 1),
    assert_extended_datatype([{enum, [never, always]}, {duration, s}], "1s", 1),
    assert_extended_datatype([{atom, never}, {atom, always}], "foo", {error, transform_datatypes, "Error transforming datatype for: a.b"}),
    ok.

not_found_test() ->
    Mappings = [cuttlefish_mapping:parse({mapping, "a", "b.c", []})],
    Translations = [cuttlefish_translation:parse(
        {translation, "b.c",
        fun(Conf) -> cuttlefish:conf_get("d", Conf) end})
    ],
    AppConf = map({Translations, Mappings, []}, [{["a"], "foo"}]),
    ?assertEqual({error, apply_translations,
                  {errorlist,
                   [{error, {translation_missing_setting,
                             {"b.c", "d"}}}]}},
                 AppConf).

invalid_test() ->
    Mappings = [cuttlefish_mapping:parse({mapping, "a", "b.c", []})],
    Translations = [cuttlefish_translation:parse(
        {translation, "b.c",
        fun(_Conf) -> cuttlefish:invalid("review all files") end})
    ],
    AppConf = map({Translations, Mappings, []}, [{["a"], "foo"}]),
    ?assertEqual({error, apply_translations,
                  {errorlist,
                   [{error, {translation_invalid_configuration,
                             {"b.c", "review all files"}}}]}},
                 AppConf).

value_env_sub_test() ->
    os:putenv("ENV_TEST_VAL", "/a/b"),
    Conf = [
            {["a","b","c"], "$(ENV_TEST_VAL)/c"},
            {["a","b"], "/b/a"}
           ],
    {NewConf, Errors} = value_sub(Conf),
    os:unsetenv("ENV_TEST_VAL"),
    ?assertEqual([], Errors),
    ABC = proplists:get_value(["a","b","c"], NewConf),
    ?assertEqual("/a/b/c", ABC),
    ok.

value_sub_test() ->
    Conf = [
            {["a","b","c"], "$(a.b)/c"},
            {["a","b"], "/a/b"}
           ],
    {NewConf, Errors} = value_sub(Conf),
    ?assertEqual([], Errors),
    ABC = proplists:get_value(["a","b","c"], NewConf),
    ?assertEqual("/a/b/c", ABC),
    ok.

value_sub_infinite_loop_test() ->
    Conf = [
            {["a"], "$(c)/d"},
            {["b"], "$(a)/d"},
            {["c"], "$(b)/d"}
           ],
    {_NewConf, Errors} = value_sub(Conf),
    ?assertEqual(
       "Circular RHS substitutions: [[\"a\"],[\"b\"],[\"c\"],[\"a\"]]",
       ?XLATE(hd(Errors))
      ),
    ?assertEqual(
       "Circular RHS substitutions: [[\"b\"],[\"c\"],[\"a\"],[\"b\"]]",
       ?XLATE(hd(tl(Errors)))
      ),
    ?assertEqual(
       "Circular RHS substitutions: [[\"c\"],[\"a\"],[\"b\"],[\"c\"]]",
       ?XLATE(hd(tl(tl(Errors))))
      ),
    ok.

value_sub_not_found_test() ->
    Conf = [
            {["a"], "$(b)/c"}
           ],
    {_NewConf, Errors} = value_sub(Conf),
    ?assertEqual(
       "'a' substitution requires a config variable 'b' to be set",
       ?XLATE(hd(Errors))
      ),
    ok.

value_sub_whitespace_test() ->
    Conf = [
            {["a", "b", "c"], "/tyktorp"},
            {["a"], "$(a.b.c)/svagen"},
            {["b"], "$(  a.b.c)/svagen"},
            {["c"], "$(a.b.c  )/svagen"},
            {["d"], "$(  a.b.c )/svagen"}
           ],
    {NewConf, []} = value_sub(Conf),
    ?assertEqual("/tyktorp/svagen", proplists:get_value(["a"], NewConf)),
    ?assertEqual("/tyktorp/svagen", proplists:get_value(["b"], NewConf)),
    ?assertEqual("/tyktorp/svagen", proplists:get_value(["c"], NewConf)),
    ?assertEqual("/tyktorp/svagen", proplists:get_value(["d"], NewConf)),
    ok.

value_sub_multiple_sub_test() ->
    Conf = [
            {["a"], "/a"},
            {["b"], "/b"},
            {["c"], "$(a)$(b)"}
           ],
    {NewConf, []} = value_sub(Conf),
    ?assertEqual("/a/b", proplists:get_value(["c"], NewConf)),
    ok.

value_sub_error_in_second_sub_test() ->
    Conf = [
            {["a"], "$(b)/$(c)"},
            {["b"], "/b"},
            {["c"], "$(a)/c"}
           ],
    {_NewConf, Errors} = value_sub(Conf),
    ?assertEqual(
       "Circular RHS substitutions: [[\"a\"],[\"c\"],[\"a\"]]",
       ?XLATE(hd(Errors))
      ),
    ?assertEqual(
       "Circular RHS substitutions: [[\"c\"],[\"a\"],[\"c\"]]",
       ?XLATE(hd(tl(Errors)))
      ),
    ok.

value_sub_false_circle_test() ->
    Conf = [
            {["a"], "$(c)/$(c)"},
            {["c"], "C"}
           ],
    {NewConf, Errors} = value_sub(Conf),
    ?assertEqual([], Errors),
    ?assertEqual("C/C", proplists:get_value(["a"], NewConf)),
    ok.

value_sub_paren_test() ->
    Conf = [
            {["a"], "$(c)/$(c)"},
            {["c"], "C)"}
           ],
    {NewConf, Errors} = value_sub(Conf),
    ?assertEqual([], Errors),
    ?assertEqual("C)/C)", proplists:get_value(["a"], NewConf)),
    ok.

%% Alias resolution tests

resolve_alias_only_alias_set_test() ->
    Mapping = cuttlefish_mapping:parse({mapping, "new.key", "app.setting", [
        {datatype, integer}, {aliases, ["old.key"]}
    ]}),
    Conf = [{["old", "key"], "42"}],
    Resolved = resolve_aliases(Conf, [Mapping]),
    ?assertEqual([{["new", "key"], "42"}], Resolved).

resolve_alias_canonical_wins_test() ->
    Mapping = cuttlefish_mapping:parse({mapping, "new.key", "app.setting", [
        {datatype, integer}, {aliases, ["old.key"]}
    ]}),
    Conf = [{["new", "key"], "100"}, {["old", "key"], "42"}],
    Resolved = resolve_aliases(Conf, [Mapping]),
    ?assertEqual([{["new", "key"], "100"}], Resolved).

resolve_alias_neither_set_test() ->
    Mapping = cuttlefish_mapping:parse({mapping, "new.key", "app.setting", [
        {datatype, integer}, {default, 7}, {aliases, ["old.key"]}
    ]}),
    ?assertEqual([], resolve_aliases([], [Mapping])).

resolve_alias_multiple_first_wins_test() ->
    %% Two aliases set, canonical not set. First alias wins,
    %% second sees canonical now exists and is dropped.
    Mapping = cuttlefish_mapping:parse({mapping, "new.key", "app.setting", [
        {datatype, integer}, {aliases, ["old.key", "oldest.key"]}
    ]}),
    Conf = [{["old", "key"], "1"}, {["oldest", "key"], "2"}],
    Resolved = resolve_aliases(Conf, [Mapping]),
    ?assertEqual([{["new", "key"], "1"}], Resolved).

resolve_alias_empty_mappings_test() ->
    Conf = [{["a", "b"], "val"}],
    ?assertEqual(Conf, resolve_aliases(Conf, [])).

resolve_alias_no_aliases_passthrough_test() ->
    Mapping = cuttlefish_mapping:parse({mapping, "a.b", "app.x", [
        {datatype, string}
    ]}),
    Conf = [{["a", "b"], "val"}, {["c", "d"], "other"}],
    ?assertEqual(Conf, resolve_aliases(Conf, [Mapping])).

resolve_alias_preserves_unrelated_keys_test() ->
    Mapping = cuttlefish_mapping:parse({mapping, "new.key", "app.setting", [
        {datatype, integer}, {aliases, ["old.key"]}
    ]}),
    Conf = [{["old", "key"], "42"}, {["unrelated", "key"], "hello"}],
    Resolved = resolve_aliases(Conf, [Mapping]),
    ?assertEqual("hello", proplists:get_value(["unrelated", "key"], Resolved)),
    ?assertEqual("42", proplists:get_value(["new", "key"], Resolved)),
    ?assertEqual(2, length(Resolved)).

resolve_alias_multiple_independent_mappings_test() ->
    %% Two mappings, each with their own alias. No interaction.
    M1 = cuttlefish_mapping:parse({mapping, "new.a", "app.x", [
        {datatype, integer}, {aliases, ["old.a"]}
    ]}),
    M2 = cuttlefish_mapping:parse({mapping, "new.b", "app.y", [
        {datatype, string}, {aliases, ["old.b"]}
    ]}),
    Conf = [{["old", "a"], "1"}, {["old", "b"], "hello"}],
    Resolved = resolve_aliases(Conf, [M1, M2]),
    ?assertEqual("1", proplists:get_value(["new", "a"], Resolved)),
    ?assertEqual("hello", proplists:get_value(["new", "b"], Resolved)),
    ?assertEqual(2, length(Resolved)).

resolve_alias_warns_on_deprecated_key_test() ->
    _ = cuttlefish_test_logging:set_up(),
    _ = cuttlefish_test_logging:bounce(warning),
    Mapping = cuttlefish_mapping:parse({mapping, "new.key", "app.setting", [
        {datatype, integer}, {aliases, ["old.key"]}
    ]}),
    _ = resolve_aliases([{["old", "key"], "42"}], [Mapping]),
    [Log] = cuttlefish_test_logging:get_logs(),
    ?assertMatch({match, _},
        re:run(Log, "old\\.key is deprecated, use new\\.key instead")).

resolve_alias_warns_on_ignored_key_test() ->
    _ = cuttlefish_test_logging:set_up(),
    _ = cuttlefish_test_logging:bounce(warning),
    Mapping = cuttlefish_mapping:parse({mapping, "new.key", "app.setting", [
        {datatype, integer}, {aliases, ["old.key"]}
    ]}),
    _ = resolve_aliases([{["new", "key"], "1"}, {["old", "key"], "2"}], [Mapping]),
    [Log] = cuttlefish_test_logging:get_logs(),
    ?assertMatch({match, _},
        re:run(Log, "old\\.key is deprecated and ignored in favor of new\\.key")).

resolve_alias_multiple_all_warn_test() ->
    _ = cuttlefish_test_logging:set_up(),
    _ = cuttlefish_test_logging:bounce(warning),
    Mapping = cuttlefish_mapping:parse({mapping, "new.key", "app.setting", [
        {datatype, integer}, {aliases, ["old.key", "oldest.key"]}
    ]}),
    _ = resolve_aliases([{["old", "key"], "1"}, {["oldest", "key"], "2"}], [Mapping]),
    Logs = cuttlefish_test_logging:get_logs(),
    ?assertEqual(2, length(Logs)).

%% End-to-end tests through map/2

alias_end_to_end_test() ->
    Schema = cuttlefish_schema:strings([
        "{mapping, \"new.key\", \"app.setting\", [\n"
        "  {datatype, integer},\n"
        "  {default, 10},\n"
        "  {aliases, [\"old.key\"]}\n"
        "]}.\n"
    ]),
    Conf = [{["old", "key"], "42"}],
    Result = map(Schema, Conf),
    ?assertEqual(42, proplists:get_value(setting, proplists:get_value(app, Result))).

alias_with_translation_test() ->
    %% Translation sees the canonical key, never the alias.
    Schema = cuttlefish_schema:strings([
        "{mapping, \"new.a\", \"app.combined\", [\n"
        "  {datatype, integer},\n"
        "  {aliases, [\"old.a\"]}\n"
        "]}.\n"
        "{mapping, \"new.b\", \"app.combined\", [\n"
        "  {datatype, integer}\n"
        "]}.\n"
        "{translation, \"app.combined\", fun(Conf) ->\n"
        "    A = cuttlefish:conf_get(\"new.a\", Conf),\n"
        "    B = cuttlefish:conf_get(\"new.b\", Conf),\n"
        "    {A, B}\n"
        "end}.\n"
    ]),
    Conf = [{["old", "a"], "1"}, {["new", "b"], "2"}],
    Result = map(Schema, Conf),
    ?assertEqual({1, 2}, proplists:get_value(combined, proplists:get_value(app, Result))).

alias_default_applies_when_neither_set_test() ->
    Schema = cuttlefish_schema:strings([
        "{mapping, \"new.key\", \"app.setting\", [\n"
        "  {datatype, integer},\n"
        "  {default, 99},\n"
        "  {aliases, [\"old.key\"]}\n"
        "]}.\n"
    ]),
    Result = map(Schema, []),
    ?assertEqual(99, proplists:get_value(setting, proplists:get_value(app, Result))).

alias_validation_runs_test() ->
    Schema = cuttlefish_schema:strings([
        "{mapping, \"new.key\", \"app.setting\", [\n"
        "  {datatype, integer},\n"
        "  {aliases, [\"old.key\"]},\n"
        "  {validators, [\"positive\"]}\n"
        "]}.\n"
        "{validator, \"positive\", \"must be positive\",\n"
        "  fun(X) -> X > 0 end}.\n"
    ]),
    Conf = [{["old", "key"], "-5"}],
    Result = map(Schema, Conf),
    ?assertMatch({error, validation, _}, Result).

minimal_map_resolves_alias_test() ->
    Schema = cuttlefish_schema:strings([
        "{mapping, \"new.key\", \"app.setting\", [\n"
        "  {datatype, integer},\n"
        "  {default, 10},\n"
        "  {aliases, [\"old.key\"]}\n"
        "]}.\n"
    ]),
    Conf = [{["old", "key"], "42"}],
    Result = minimal_map(Schema, Conf),
    ?assertEqual(42, proplists:get_value(setting, proplists:get_value(app, Result))).

minimal_map_excludes_mapping_without_alias_or_canonical_test() ->
    Schema = cuttlefish_schema:strings([
        "{mapping, \"new.key\", \"app.setting\", [\n"
        "  {datatype, integer},\n"
        "  {default, 10},\n"
        "  {aliases, [\"old.key\"]}\n"
        "]}.\n"
    ]),
    %% Neither canonical nor alias in config — mapping should be excluded
    Result = minimal_map(Schema, []),
    ?assertEqual([], Result).

%% RHS substitutions reference the canonical key, not the alias.
%% $(old.key) won't resolve after alias rewriting — use $(new.key).
rhs_substitution_uses_canonical_not_alias_test() ->
    Schema = cuttlefish_schema:strings([
        "{mapping, \"new.key\", \"app.a\", [\n"
        "  {datatype, string},\n"
        "  {aliases, [\"old.key\"]}\n"
        "]}.\n"
        "{mapping, \"other\", \"app.b\", [\n"
        "  {datatype, string}\n"
        "]}.\n"
    ]),
    %% $(new.key) works after alias resolution
    Conf = [{["old", "key"], "hello"}, {["other"], "$(new.key)/world"}],
    Result = map(Schema, Conf),
    ?assertEqual("hello/world", proplists:get_value(b, proplists:get_value(app, Result))).

rhs_substitution_of_alias_key_fails_test() ->
    Schema = cuttlefish_schema:strings([
        "{mapping, \"new.key\", \"app.a\", [\n"
        "  {datatype, string},\n"
        "  {aliases, [\"old.key\"]}\n"
        "]}.\n"
        "{mapping, \"other\", \"app.b\", [\n"
        "  {datatype, string}\n"
        "]}.\n"
    ]),
    %% $(old.key) fails because the alias has been rewritten
    Conf = [{["old", "key"], "hello"}, {["other"], "$(old.key)/world"}],
    Result = map(Schema, Conf),
    ?assertMatch({error, rhs_subs, _}, Result).

rhs_substitution_of_alias_key_error_message_test() ->
    Schema = cuttlefish_schema:strings([
        "{mapping, \"new.key\", \"app.a\", [\n"
        "  {datatype, string},\n"
        "  {aliases, [\"old.key\"]}\n"
        "]}.\n"
        "{mapping, \"other\", \"app.b\", [\n"
        "  {datatype, string}\n"
        "]}.\n"
    ]),
    Conf = [{["old", "key"], "hello"}, {["other"], "$(old.key)/world"}],
    {error, rhs_subs, {errorlist, [{error, {substitution_alias_key, {_, MissingVar, Canonical}}}]}} =
        map(Schema, Conf),
    ?assertMatch({match, _}, re:run(cuttlefish_error:xlate({substitution_alias_key, {"other", MissingVar, Canonical}}),
                                    "alias")).

-endif.
