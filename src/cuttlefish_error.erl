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
-module(cuttlefish_error).

-include_lib("kernel/include/logger.hrl").

-type error() :: {'error', {atom(), term()}}.
-type errorlist() :: {'errorlist', [error()]}.
-export_type([error/0, errorlist/0]).

-export([
        contains_error/1,
        is_error/1,
        filter/1,
        errorlist_maybe/1,
        print/1,
        print/2,
        xlate/1
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% We'll be calling this a lot from `xlate'
-define(STR(X, Y), xlate(cuttlefish_datatypes:to_string(X, Y))).

-spec xlate({atom(), term()}|string()) -> iolist().
xlate(Message) when is_list(Message) ->
    %% We allow for strings so that we can safely call
    %% `cuttlefish_datatypes:to_string` when creating these messages
    Message;
xlate({error, Details}) ->
    xlate(Details);
xlate({_Error, {error, NestedError}}) ->
    xlate(NestedError);
xlate({type, {Value, Type}}) ->
    io_lib:format("Tried to convert ~tp but invalid datatype: ~tp",
                  [Value, Type]);
xlate({range, {{Value, Type}, Range}}) ->
    [?STR(Value, Type), " can't be outside the range ", Range];
xlate({conversion, {Value, Type}}) ->
    io_lib:format("~tp cannot be converted to a(n) ~ts", [Value, Type]);
xlate({duration, Value}) ->
    io_lib:format("Invalid duration value: ~ts", [Value]);
xlate({enum_name, {Value, EnumNames}}) ->
    io_lib:format("~tp is not a valid enum value, acceptable values are: ~ts",
                  [Value, string:join(EnumNames, ", ")]);
xlate({enum_format, Value}) ->
    %% This collapses two different type of formatting errors into one
    %% error message
    io_lib:format("Enum elements must be atoms, strings, or 2-tuples with "
                  "atom or string as first element. Bad value: ~w", [Value]);
xlate({mapping_types, List}) ->
    io_lib:format("Invalid datatype list for mapping: ~ts",
                  [string:join(List, ", ")]);
xlate({mapping_parse, Term}) ->
    io_lib:format(
        "Poorly formatted input to cuttlefish_mapping:parse/1 : ~tp",
        [Term]
     );
xlate({translation_parse, Term}) ->
    io_lib:format(
      "Poorly formatted input to cuttlefish_translation:parse/1 : ~tp",
      [Term]
     );
xlate({validator_parse, Term}) ->
    io_lib:format(
      "Poorly formatted input to cuttlefish_validator:parse/1 : ~tp",
      [Term]
     );
xlate({conf_to_unicode, LineNum}) ->
    io_lib:format("Error converting value on line #~tp to unicode", [LineNum]);
xlate({bytesize_parse, Value}) ->
    io_lib:format("Error converting value ~tp to a number of bytes", [Value]);
xlate({file_open, {File, Reason}}) ->
    io_lib:format("Could not open file (~ts) for Reason ~ts", [File, Reason]);
xlate({conf_syntax, {File, {Line, Col}}}) ->
    io_lib:format("Syntax error in ~ts after line ~tp column ~tp, "
                  "parsing incomplete", [File, Line, Col]);
xlate({in_file, {File, Error}}) ->
    [File, ": ", xlate(Error)];
xlate({translation_missing_setting, {Translation, Setting}}) ->
    io_lib:format("Translation for '~ts' expected to find setting '~ts' but was missing",
                  [Translation, Setting]);
xlate({translation_invalid_configuration, {Translation, Invalid}}) ->
    io_lib:format("Translation for '~ts' found invalid configuration: ~ts",
                  [Translation, Invalid]);
xlate({translation_unknown_error, {Translation, {Class, Error}}}) ->
    io_lib:format("Error running translation for ~ts, [~tp, ~tp]",
                  [Translation, Class, Error]);
xlate({translation_arity, {Translation, Arity}}) ->
    io_lib:format("~tp is not a valid arity for translation fun() ~ts."
                  " Try 1 or 2", [Arity, Translation]);
xlate({map_multiple_match, VariableDefinition}) ->
    io_lib:format("~tp has both a fuzzy and strict match", [VariableDefinition]);
xlate({unknown_variable, Variable}) ->
    ["Conf file attempted to set unknown variable: ", Variable];
xlate({unsupported_type, Type}) ->
    io_lib:format("~tp is not a supported datatype", [Type]);
xlate({transform_type, Type}) ->
    ["Error transforming datatype for: ", Type];
xlate({transform_type_exception, {Type, {Class, Error}}}) ->
    io_lib:format("Caught exception converting to ~tp: ~tp:~tp",
                  [Type, Class, Error]);
xlate({transform_type_unacceptable, {Value, BadValue}}) ->
    io_lib:format("~tp is not accepted value: ~tp", [Value, BadValue]);
xlate({circular_rhs, History}) ->
    io_lib:format("Circular RHS substitutions: ~tp", [History]);
xlate({substitution_missing_config, {Substitution, Variable}}) ->
    io_lib:format("'~ts' substitution requires a config variable '~ts' to be set",
                  [Substitution, Variable]);
xlate({mapping_not_found, Variable}) ->
    [Variable, " not_found"];
xlate({mapping_multiple, {Variable, {Hard, Fuzzy}}}) ->
    io_lib:format("~tp hard mappings and ~tp fuzzy mappings found "
                  "for ~ts", [Hard, Fuzzy, Variable]);
xlate({validation, {Variable, Description}}) ->
    [Variable, " invalid, ", Description];
xlate({erl_parse, {Reason, LineNo}}) ->
    ["Schema parse error near line number ", integer_to_list(LineNo),
     ": ", Reason];
xlate({erl_parse, Reason}) ->
    io_lib:format("Schema parse error: ~tp", [Reason]);
xlate({erl_parse_unexpected, Error}) ->
    io_lib:format("Unexpected return from erl_parse:parse_exprs/1: ~tp",
                  [Error]);
xlate({parse_schema, Value}) ->
    io_lib:format("Unknown parse return: ~tp", [Value]);
xlate({erl_scan, LineNo}) ->
    ["Error scanning erlang near line ", integer_to_list(LineNo)].

-spec contains_error(list()) -> boolean().
contains_error(List) ->
    lists:any(fun is_error/1, List).

-spec is_error(any()) -> boolean().
is_error({error, _}) -> true;
is_error(_) -> false.

-spec filter(list()) -> errorlist().
filter(List) ->
    {errorlist, lists:filter(fun is_error/1, List)}.

-spec errorlist_maybe(any()) -> any().
errorlist_maybe(List) when is_list(List) ->
    case filter(List) of
        {errorlist, []} ->
            List;
        Errorlist ->
            Errorlist
    end;
errorlist_maybe(AnythingElse) -> AnythingElse.

-spec print(string(), [any()]) -> ok.
print(FormatString, Args) ->
    print(io_lib:format(FormatString, Args)).

-spec print(string() | error()) -> ok.
print({error, ErrorTerm}) ->
    print(lists:flatten(xlate(ErrorTerm)));
print(String) ->
    try
        ?LOG_ERROR("~ts", [String])
    catch _:_:_ ->
        io:format("~ts~n", [String]),
        ok
    end.

-ifdef(TEST).

is_error_test() ->
    ?assert(is_error({error, "oh no!"})),
    ?assert(not(is_error("just an innocent string... I mean a list... I mean... argh, erlang"))),
    ok.

contains_error_test() ->
    ?assert(contains_error(["hi", {error, "hi!"}, "bye"])),
    ?assert(not(contains_error(["hi", "I'm not an error", "bye"]))),
    ok.

filter_test() ->
    ?assertEqual({errorlist, []}, filter(["hi", "what even is an error?", "bye"])),
    ?assertEqual({errorlist, [{error, "etoomanythings"}]},
                 filter(["hi", {error, "etoomanythings"}, "bye"])),
    ok.

errorlist_maybe_test() ->
    ?assertEqual(atom, errorlist_maybe(atom)),
    ?assertEqual(12, errorlist_maybe(12)),
    %% Fool you! "string" is a list!, but doesn't contain an error()
    ?assertEqual("string", errorlist_maybe("string")),

    ?assertEqual(
       {errorlist, [{error, "etoomanythings"}]},
       errorlist_maybe(["hi", {error, "etoomanythings"}, "bye"])),
    ?assertEqual(
       ["hi", "what even is an error?", "bye"],
       errorlist_maybe(["hi", "what even is an error?", "bye"])),
    ok.

-endif.
