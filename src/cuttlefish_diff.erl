%% -------------------------------------------------------------------
%%
%% cuttlefish_diff: deterministic rendering of generated app.config
%% proplists for line-diffing two pipeline outputs.
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

-module(cuttlefish_diff).

-export([render_normalised/1, render_normalised/2]).

-type option() :: {skip_funs, boolean()}
                | {atom_quoting, strict | loose}.
-type options() :: [option()].
-export_type([options/0]).

%% @doc Produces a deterministic, sorted, printable string for the
%% output of `cuttlefish_generator:map/2'. Two outputs that should
%% be equivalent under the schema produce identical strings; any
%% real difference shows up as a localised line-level diff.
-spec render_normalised([proplists:property()]) -> iolist().
render_normalised(Config) ->
    render_normalised(Config, []).

-spec render_normalised([proplists:property()], options()) -> iolist().
render_normalised(Config, Options) ->
    Sorted = canonicalise(Config),
    [render_top(Entry, Options) || Entry <- Sorted].

%% Top-level entries are `{App, Settings}'. Each app section header
%% goes on its own line for easy diffing, followed by indented key
%% lines, then a blank line as a separator.
render_top({App, Settings}, Options) when is_atom(App), is_list(Settings) ->
    [render_atom(App, Options), " =>\n",
     [["  ", render_pair(Pair, Options), "\n"] || Pair <- Settings],
     "\n"];
render_top(Other, Options) ->
    %% A non-`{App, Settings}' tuple at the top level is unusual but
    %% not an error; render it the same way a nested value would be.
    [render_term(Other, Options), "\n"].

render_pair({Key, Value}, Options) ->
    [render_term(Key, Options), " = ", render_term(Value, Options)];
render_pair(Other, Options) ->
    render_term(Other, Options).

%% Funs/pids/ports/refs have no stable structural form between runs.
%% Under default `{skip_funs, true}', funs collapse to `#Fun<>'; with
%% `{skip_funs, false}' the `~p' form (e.g. `#Fun<m:f/n>') gives a
%% little more detail at the cost of being less stable across nodes.
%% Pids/ports/refs always reduce to `#Opaque<>' — their printed form
%% embeds runtime identifiers and is never useful for diffing.
render_term(V, _) when is_atom(V), V =:= true -> "true";
render_term(V, _) when is_atom(V), V =:= false -> "false";
render_term(V, _) when is_atom(V), V =:= undefined -> "undefined";
render_term(V, Options) when is_atom(V) -> render_atom(V, Options);
render_term(V, _) when is_integer(V) -> integer_to_list(V);
render_term(V, _) when is_float(V)   -> float_to_list(V, [{decimals, 12}, compact]);
render_term(V, _Options) when is_binary(V) ->
    %% Distinguish a binary from a string of the same characters.
    ["<<", render_string(unicode:characters_to_list(V)), ">>"];
render_term(V, Options) when is_function(V) ->
    case proplists:get_value(skip_funs, Options, true) of
        true  -> "#Fun<>";
        false -> io_lib:format("~p", [V])
    end;
render_term(V, _) when is_pid(V); is_port(V); is_reference(V) ->
    "#Opaque<>";
render_term([], _) -> "[]";
render_term(V, Options) when is_list(V) ->
    case io_lib:printable_unicode_list(V) of
        true ->
            ["\"", render_string(V), "\""];
        false ->
            ["[",
             string:join([lists:flatten(render_term(E, Options)) || E <- V], ", "),
             "]"]
    end;
render_term(V, Options) when is_tuple(V) ->
    Elements = tuple_to_list(V),
    ["{",
     string:join([lists:flatten(render_term(E, Options)) || E <- Elements], ", "),
     "}"];
render_term(V, Options) when is_map(V) ->
    Sorted = lists:sort(maps:to_list(V)),
    ["#{",
     string:join(
       [lists:flatten([render_term(K, Options), " => ",
                       render_term(Val, Options)])
        || {K, Val} <- Sorted],
       ", "),
     "}"].

render_atom(A, Options) ->
    case proplists:get_value(atom_quoting, Options, loose) of
        strict ->
            io_lib:format("~p", [A]);
        _ ->
            atom_to_list(A)
    end.

render_string(S) ->
    %% Escape backslash and double-quote so the output is unambiguous,
    %% but leave other printable characters alone.
    lists:flatten([escape_char(C) || C <- S]).

escape_char($\\) -> "\\\\";
escape_char($")  -> "\\\"";
escape_char($\n) -> "\\n";
escape_char($\r) -> "\\r";
escape_char($\t) -> "\\t";
escape_char(C)   -> [C].

%% Walks the config and sorts nested proplists by key. This is what
%% makes two equivalent outputs produce the same string regardless
%% of original key order.
canonicalise(Config) when is_list(Config) ->
    Items = [canonicalise_top(I) || I <- Config],
    lists:sort(Items).

canonicalise_top({App, Settings}) when is_atom(App), is_list(Settings) ->
    {App, sort_proplist(Settings)};
canonicalise_top(Other) ->
    Other.

sort_proplist(L) when is_list(L) ->
    Items = lists:map(fun sort_value/1, L),
    lists:sort(fun compare_pairs/2, Items);
sort_proplist(Other) -> Other.

sort_value({K, V}) -> {K, walk(V)};
sort_value(V) -> walk(V).

walk(V) when is_list(V) ->
    case looks_like_proplist(V) of
        true  -> sort_proplist(V);
        false -> [walk(E) || E <- V]
    end;
walk(V) when is_tuple(V) ->
    list_to_tuple([walk(E) || E <- tuple_to_list(V)]);
walk(V) when is_map(V) ->
    maps:from_list([{K, walk(Val)} || {K, Val} <- maps:to_list(V)]);
walk(Other) -> Other.

%% Only re-sort a list when every element is a 2-tuple — a real
%% proplist. A mixed list (including all-atom flag lists) keeps its
%% original order, since reordering a TLS cipher list or a similar
%% ordered collection would silently corrupt config semantics.
looks_like_proplist([]) -> false;
looks_like_proplist(L) ->
    lists:all(fun({_, _}) -> true; (_) -> false end, L).

compare_pairs({A, _}, {B, _}) -> A =< B;
compare_pairs(A, B) -> A =< B.
