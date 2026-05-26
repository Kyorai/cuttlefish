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
xlate({substitution_alias_key, {Substitution, Alias, Canonical}}) ->
    io_lib:format("'~ts' substitution references '~ts', which is a deprecated alias for '~ts'",
                  [Substitution, Alias, Canonical]);
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
    ["Error scanning erlang near line ", integer_to_list(LineNo)];
xlate({aliases_empty, Variable}) ->
    io_lib:format("No aliases declared for mapping ~ts (omit the aliases property instead)", [Variable]);
xlate({aliases_is_bare_string, Variable}) ->
    io_lib:format("Aliases value for mapping ~ts is a bare string; wrap it in a list, e.g. {aliases, [\"old.key\"]}", [Variable]);
xlate({alias_not_a_string, Variable, Value}) ->
    io_lib:format("Alias for mapping ~ts must be a string, got ~tp", [Variable, Value]);
xlate({alias_and_aliases_both_set, Variable}) ->
    io_lib:format("Mapping ~ts declares both {alias, ...} and {aliases, ...} (use one or the other)", [Variable]);
xlate({alias_is_self, Variable}) ->
    io_lib:format("Mapping ~ts lists itself among its aliases", [Variable]);
xlate({aliases_contain_duplicates, Variable}) ->
    io_lib:format("Mapping ~ts has duplicate entries in its aliases list", [Variable]);
xlate({aliases_invalid_value, Variable, Value}) ->
    io_lib:format("Aliases for mapping ~ts must be a list of strings, got ~tp", [Variable, Value]);
xlate({fuzzy_alias_unsupported, Variable, Alias}) ->
    io_lib:format("Fuzzy alias ~ts on mapping ~ts is not supported",
                  [Alias, Variable]);
xlate({alias_shadows_canonical, {Alias, OwnerMapping}}) ->
    io_lib:format("Alias ~ts on mapping ~ts shadows the canonical variable ~ts",
                  [Alias, OwnerMapping, Alias]);
xlate({alias_claimed_by_multiple_mappings, {Alias, Mapping1, Mapping2}}) ->
    io_lib:format("Alias ~ts is claimed by both ~ts and ~ts",
                  [Alias, Mapping1, Mapping2]);
xlate({unsupported_collect_type, {proplist, binary}}) ->
    "collect type {proplist, binary} is not supported; "
    "use {proplist, atom} for atom keys or {map, binary} for binary keys";
xlate({invalid_collect_type, Value}) ->
    io_lib:format("Invalid collect type ~tp. Valid types: list, {map, atom}, "
                  "{map, binary}, {proplist, atom}", [Value]);
xlate({collect_on_non_fuzzy, Variable}) ->
    io_lib:format("collect property on mapping ~ts requires a wildcard ($) segment "
                  "in the variable name", [Variable]);
xlate({collect_multi_wildcard, Variable, N}) ->
    io_lib:format("collect property on mapping ~ts has ~B wildcard segments; "
                  "only a single wildcard is supported", [Variable, N]);
xlate({regex_empty, _}) ->
    "Regular expression cannot be empty";
xlate({regex_invalid_syntax, {Value, Reason, Pos}}) ->
    io_lib:format("~tp is not a valid regular expression (~ts at position ~B)",
                  [Value, Reason, Pos]);
xlate({regex_excessive_backtracking, Value}) ->
    io_lib:format("~tp is prone to excessive backtracking; "
                  "simplify the pattern or avoid nested or overlapping quantifiers",
                  [Value]);
xlate({uri_empty, _}) ->
    "URI value cannot be empty";
xlate({uri_malformed, Value}) ->
    io_lib:format("~tp is not a parseable URI", [Value]);
xlate({uri_no_scheme, Value}) ->
    io_lib:format("URI ~tp is missing a scheme (e.g. https://)", [Value]);
xlate({uri_no_host, Value}) ->
    io_lib:format("URI ~tp is missing a host", [Value]);
xlate({uri_bad_scheme, {Value, Got, Allowed}}) ->
    AllowedList = string:join([atom_to_list(S) || S <- Allowed], ", "),
    io_lib:format("URI ~tp uses scheme '~ts'; expected one of: ~ts",
                  [Value, Got, AllowedList]);
xlate({uri_schemes_empty, _}) ->
    "URI scheme list cannot be empty";
xlate({uri_schemes_invalid, Schemes}) ->
    io_lib:format("URI schemes must be a non-empty list of atoms, got ~tp",
                  [Schemes]);
xlate({range_violation, {Value, {min, Bound}}}) ->
    io_lib:format("~tp is below the minimum allowed value of ~tp",
                  [Value, Bound]);
xlate({range_violation, {Value, {max, Bound}}}) ->
    io_lib:format("~tp exceeds the maximum allowed value of ~tp",
                  [Value, Bound]);
xlate({range_violation, {Value, {gt, Bound}}}) ->
    io_lib:format("~tp must be strictly greater than ~tp", [Value, Bound]);
xlate({range_violation, {Value, {lt, Bound}}}) ->
    io_lib:format("~tp must be strictly less than ~tp", [Value, Bound]);
xlate({schema_file, Filename, Inner}) ->
    ["in schema file \"", Filename, "\": ", xlate(Inner)];
xlate({not_a_schema_file, Filename}) ->
    io_lib:format("refused to parse ~ts: not a schema file "
                  "(expected a .schema extension)", [Filename]);
xlate({schema_file_too_large, {Filename, Size, Limit}}) ->
    io_lib:format("refused to parse ~ts: file size ~B bytes exceeds the "
                  "schema size limit of ~B bytes", [Filename, Size, Limit]);
xlate({schema_file_unrecognized_content, Filename}) ->
    io_lib:format("refused to parse ~ts: content does not look like a "
                  "cuttlefish schema (expected a top-level mapping, "
                  "translation, or validator tuple)", [Filename]);
xlate({schema_file_read_error, {Filename, Reason}}) ->
    io_lib:format("could not read schema file ~ts: ~tp", [Filename, Reason]);
xlate({schema_file_invalid_unicode, Filename}) ->
    io_lib:format("schema file ~ts is not valid UTF-8", [Filename]).

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
    %% A logger with no handler silently drops `?LOG_ERROR'; write
    %% to `standard_error' too so misconfiguration is never silent.
    io:format(standard_error, "~ts~n", [String]),
    catch ?LOG_ERROR("~ts", [String]),
    ok.

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

%% Alias error xlate tests

alias_error_xlate_test() ->
    ?assertEqual("No aliases declared for mapping a.b (omit the aliases property instead)",
                 lists:flatten(xlate({aliases_empty, "a.b"}))),
    ?assertEqual("Aliases value for mapping a.b is a bare string; "
                 "wrap it in a list, e.g. {aliases, [\"old.key\"]}",
                 lists:flatten(xlate({aliases_is_bare_string, "a.b"}))),
    ?assertEqual("Mapping a.b lists itself among its aliases",
                 lists:flatten(xlate({alias_is_self, "a.b"}))),
    ?assertEqual("Fuzzy alias old.$name.key on mapping a.b is not supported",
                 lists:flatten(xlate({fuzzy_alias_unsupported, "a.b", "old.$name.key"}))),
    ?assertEqual("Alias for mapping a.b must be a string, got 42",
                 lists:flatten(xlate({alias_not_a_string, "a.b", 42}))),
    ?assertEqual("Mapping a.b has duplicate entries in its aliases list",
                 lists:flatten(xlate({aliases_contain_duplicates, "a.b"}))),
    ?assertEqual("Aliases for mapping a.b must be a list of strings, got 42",
                 lists:flatten(xlate({aliases_invalid_value, "a.b", 42}))),
    ?assertEqual("Mapping a.b declares both {alias, ...} and {aliases, ...} "
                 "(use one or the other)",
                 lists:flatten(xlate({alias_and_aliases_both_set, "a.b"}))),
    ?assertEqual("Alias c.d on mapping a.b shadows the canonical variable c.d",
                 lists:flatten(xlate({alias_shadows_canonical, {"c.d", "a.b"}}))),
    ?assertEqual("Alias old.key is claimed by both a.b and c.d",
                 lists:flatten(xlate({alias_claimed_by_multiple_mappings, {"old.key", "a.b", "c.d"}}))).

%% Schema-file error xlate tests for the tags introduced by the
%% non-schema-file candidates.

schema_file_wrap_xlate_test() ->
    Inner = {error, {erl_scan, 32230}},
    ?assertEqual(
       "in schema file \"/tmp/erl_crash.dump\": "
       "Error scanning erlang near line 32230",
       lists:flatten(xlate({schema_file, "/tmp/erl_crash.dump", Inner}))),
    ok.

not_a_schema_file_xlate_test() ->
    ?assertEqual(
       "refused to parse /tmp/erl_crash.dump: not a schema file "
       "(expected a .schema extension)",
       lists:flatten(xlate({not_a_schema_file, "/tmp/erl_crash.dump"}))),
    ok.

schema_file_too_large_xlate_test() ->
    ?assertEqual(
       "refused to parse /tmp/big.schema: file size 16777216 bytes "
       "exceeds the schema size limit of 8388608 bytes",
       lists:flatten(xlate({schema_file_too_large,
                            {"/tmp/big.schema", 16777216, 8388608}}))),
    ok.

schema_file_unrecognized_content_xlate_test() ->
    ?assertEqual(
       "refused to parse /tmp/foo.schema: content does not look like a "
       "cuttlefish schema (expected a top-level mapping, translation, "
       "or validator tuple)",
       lists:flatten(xlate({schema_file_unrecognized_content, "/tmp/foo.schema"}))),
    ok.

schema_file_read_error_xlate_test() ->
    ?assertEqual(
       "could not read schema file /missing.schema: not_readable",
       lists:flatten(xlate({schema_file_read_error,
                            {"/missing.schema", not_readable}}))),
    ok.

schema_file_invalid_unicode_xlate_test() ->
    ?assertEqual(
       "schema file /tmp/garbage.schema is not valid UTF-8",
       lists:flatten(xlate({schema_file_invalid_unicode, "/tmp/garbage.schema"}))),
    ok.

%% Re-register `standard_error' to a temp file so we can verify
%% `print/1' reaches it without a logger handler.
print_writes_to_standard_error_test() ->
    StderrFile = filename:join(cuttlefish_paths:tmp_base(),
                               "cuttlefish_print_stderr.txt"),
    {ok, F} = file:open(StderrFile, [write]),
    OldStderr = whereis(standard_error),
    true = unregister(standard_error),
    true = register(standard_error, F),
    try
        print("hello from stderr")
    after
        true = unregister(standard_error),
        true = register(standard_error, OldStderr),
        file:close(F)
    end,
    {ok, Bytes} = file:read_file(StderrFile),
    file:delete(StderrFile),
    ?assert(binary:match(Bytes, <<"hello from stderr">>) =/= nomatch),
    ok.

-endif.
