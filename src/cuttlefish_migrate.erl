%% -------------------------------------------------------------------
%%
%% cuttlefish_migrate: rewrites alias keys to canonical names in a
%% conf_file, optionally transforming values via migration functions.
%%
%% Copyright (c) 2025 Broadcom. All Rights Reserved. The term
%% "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
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
-module(cuttlefish_migrate).

-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    build_alias_map/1,
    run/2
]).

-type alias_entry() :: {Canonical :: string(),
                        MigrationFun :: fun((string()) -> string()) | undefined,
                        Message :: string() | undefined}.
-type alias_map() :: #{string() => alias_entry()}.
-type change() :: {rename, OldKey :: string(), NewKey :: string()}
                | {rename, OldKey :: string(), NewKey :: string(), OldValue :: string(), NewValue :: string()}
                | {conflict, AliasKey :: string(), CanonicalKey :: string()}.
-export_type([alias_map/0, change/0]).

-spec build_alias_map([cuttlefish_mapping:mapping()]) -> alias_map().
build_alias_map(Mappings) ->
    lists:foldl(fun(M, Acc) ->
        Canonical = cuttlefish_variable:format(cuttlefish_mapping:variable(M)),
        MigrationFun = cuttlefish_mapping:migration(M),
        lists:foldl(fun({AliasVar, Msg}, InnerAcc) ->
            AliasStr = cuttlefish_variable:format(AliasVar),
            maps:put(AliasStr, {Canonical, MigrationFun, Msg}, InnerAcc)
        end, Acc, cuttlefish_mapping:aliases_with_messages(M))
    end, #{}, Mappings).

-spec run(cuttlefish_conf_file:conf_file(), alias_map()) ->
    {cuttlefish_conf_file:conf_file(), [change()]}.
run(CF, AliasMap) ->
    Keys = cuttlefish_conf_file:keys(CF),
    {ResultCF, RevChanges} = lists:foldl(fun(Key, {CFAcc, ChangesAcc}) ->
        case maps:find(Key, AliasMap) of
            {ok, {Canonical, MigrationFun, _Msg}} ->
                migrate_key(CFAcc, ChangesAcc, Key, Canonical, MigrationFun);
            error ->
                {CFAcc, ChangesAcc}
        end
    end, {CF, []}, Keys),
    {ResultCF, lists:reverse(RevChanges)}.

%% ===================================================================
%% Internal
%% ===================================================================

migrate_key(CF, Changes, AliasKey, CanonicalKey, MigrationFun) ->
    case cuttlefish_conf_file:get(CF, CanonicalKey) of
        {ok, _} ->
            CF2 = cuttlefish_conf_file:add_comment_before(CF, AliasKey,
                      "Conflict: " ++ AliasKey ++
                      " is superseded by " ++ CanonicalKey),
            CF3 = cuttlefish_conf_file:comment_out(CF2, AliasKey),
            {CF3, [{conflict, AliasKey, CanonicalKey} | Changes]};
        undefined ->
            {ok, Value} = cuttlefish_conf_file:get(CF, AliasKey),
            {NewValue, ValueChanged} = apply_migration_fun(AliasKey, Value, MigrationFun),
            CF2 = cuttlefish_conf_file:rename_key(CF, AliasKey, CanonicalKey),
            CF3 = case ValueChanged of
                true  -> cuttlefish_conf_file:set(CF2, CanonicalKey, NewValue);
                false -> CF2
            end,
            CF4 = cuttlefish_conf_file:add_comment_before(CF3, CanonicalKey,
                      "Migrated: " ++ AliasKey ++ " -> " ++ CanonicalKey),
            Change = case ValueChanged of
                true  -> {rename, AliasKey, CanonicalKey, Value, NewValue};
                false -> {rename, AliasKey, CanonicalKey}
            end,
            {CF4, [Change | Changes]}
    end.

apply_migration_fun(_Key, Value, undefined) ->
    {Value, false};
apply_migration_fun(Key, Value, Fun) ->
    try
        case Fun(Value) of
            Value    -> {Value, false};
            NewValue -> {NewValue, true}
        end
    catch
        Class:Error ->
            _ = ?LOG_ERROR("~ts", [cuttlefish_error:xlate(
                    {migration_fun_error, {Key, Class, Error}})]),
            {Value, false}
    end.

%% ===================================================================
%% Tests
%% ===================================================================

-ifdef(TEST).

build_alias_map_empty_test() ->
    ?assertEqual(#{}, build_alias_map([])).

build_alias_map_no_aliases_test() ->
    M = cuttlefish_mapping:parse({mapping, "a.b", "app.x", [{datatype, integer}]}),
    ?assertEqual(#{}, build_alias_map([M])).

build_alias_map_single_test() ->
    M = cuttlefish_mapping:parse({mapping, "new.key", "app.x", [
        {datatype, integer}, {aliases, ["old.key"]}
    ]}),
    Map = build_alias_map([M]),
    ?assertEqual(1, maps:size(Map)),
    ?assertMatch({_, {"new.key", undefined, undefined}}, maps:find("old.key", Map)).

build_alias_map_with_message_test() ->
    M = cuttlefish_mapping:parse({mapping, "new.key", "app.x", [
        {datatype, integer}, {aliases, [{"old.key", "renamed in 4.0"}]}
    ]}),
    Map = build_alias_map([M]),
    ?assertMatch({_, {"new.key", undefined, "renamed in 4.0"}}, maps:find("old.key", Map)).

build_alias_map_multiple_mappings_test() ->
    M1 = cuttlefish_mapping:parse({mapping, "new.a", "app.x", [
        {datatype, integer}, {aliases, ["old.a"]}
    ]}),
    M2 = cuttlefish_mapping:parse({mapping, "new.b", "app.y", [
        {datatype, string}, {aliases, ["old.b", "oldest.b"]}
    ]}),
    Map = build_alias_map([M1, M2]),
    ?assertEqual(3, maps:size(Map)).

run_simple_rename_test() ->
    CF = cuttlefish_conf_file:parse("old.key = 42\n"),
    AliasMap = #{"old.key" => {"new.key", undefined, undefined}},
    {Result, Changes} = run(CF, AliasMap),
    ?assertEqual({ok, "42"}, cuttlefish_conf_file:get(Result, "new.key")),
    ?assertEqual(undefined, cuttlefish_conf_file:get(Result, "old.key")),
    ?assertMatch([{rename, "old.key", "new.key"}], Changes).

run_no_match_test() ->
    CF = cuttlefish_conf_file:parse("unrelated = val\n"),
    AliasMap = #{"old.key" => {"new.key", undefined, undefined}},
    {Result, Changes} = run(CF, AliasMap),
    ?assertEqual([], Changes),
    ?assertEqual({ok, "val"}, cuttlefish_conf_file:get(Result, "unrelated")).

run_conflict_test() ->
    CF = cuttlefish_conf_file:parse("new.key = 100\nold.key = 42\n"),
    AliasMap = #{"old.key" => {"new.key", undefined, undefined}},
    {Result, Changes} = run(CF, AliasMap),
    ?assertEqual({ok, "100"}, cuttlefish_conf_file:get(Result, "new.key")),
    ?assertEqual(undefined, cuttlefish_conf_file:get(Result, "old.key")),
    ?assertMatch([{conflict, "old.key", "new.key"}], Changes).

run_with_migration_fun_test() ->
    MigFun = fun("warn") -> "warning"; (V) -> V end,
    CF = cuttlefish_conf_file:parse("old.level = warn\n"),
    AliasMap = #{"old.level" => {"new.level", MigFun, undefined}},
    {Result, Changes} = run(CF, AliasMap),
    ?assertEqual({ok, "warning"}, cuttlefish_conf_file:get(Result, "new.level")),
    ?assertMatch([{rename, "old.level", "new.level", "warn", "warning"}], Changes).

run_migration_fun_identity_test() ->
    MigFun = fun(V) -> V end,
    CF = cuttlefish_conf_file:parse("old.key = same\n"),
    AliasMap = #{"old.key" => {"new.key", MigFun, undefined}},
    {Result, Changes} = run(CF, AliasMap),
    ?assertEqual({ok, "same"}, cuttlefish_conf_file:get(Result, "new.key")),
    ?assertMatch([{rename, "old.key", "new.key"}], Changes).

run_migration_comment_test() ->
    CF = cuttlefish_conf_file:parse("old.key = val\n"),
    AliasMap = #{"old.key" => {"new.key", undefined, undefined}},
    {Result, _} = run(CF, AliasMap),
    Output = cuttlefish_conf_file:to_string(Result),
    ?assertMatch({match, _}, re:run(Output, "# Migrated: old\\.key -> new\\.key")).

run_conflict_comment_test() ->
    CF = cuttlefish_conf_file:parse("new.key = 1\nold.key = 2\n"),
    AliasMap = #{"old.key" => {"new.key", undefined, undefined}},
    {Result, _} = run(CF, AliasMap),
    Output = cuttlefish_conf_file:to_string(Result),
    ?assertMatch({match, _}, re:run(Output, "# Conflict")),
    ?assertMatch({match, _}, re:run(Output, "# old\\.key = 2")).

run_idempotent_test() ->
    CF = cuttlefish_conf_file:parse("old.key = 42\n"),
    AliasMap = #{"old.key" => {"new.key", undefined, undefined}},
    {Once, _} = run(CF, AliasMap),
    {Twice, Changes2} = run(Once, AliasMap),
    ?assertEqual([], Changes2),
    ?assertEqual(cuttlefish_conf_file:to_string(Once),
                 cuttlefish_conf_file:to_string(Twice)).

run_multiple_aliases_test() ->
    CF = cuttlefish_conf_file:parse("old.a = 1\nold.b = 2\n"),
    AliasMap = #{
        "old.a" => {"new.a", undefined, undefined},
        "old.b" => {"new.b", undefined, undefined}
    },
    {Result, Changes} = run(CF, AliasMap),
    ?assertEqual(2, length(Changes)),
    ?assertEqual({ok, "1"}, cuttlefish_conf_file:get(Result, "new.a")),
    ?assertEqual({ok, "2"}, cuttlefish_conf_file:get(Result, "new.b")).

run_migration_fun_crash_test() ->
    CrashFun = fun(_) -> error(boom) end,
    CF = cuttlefish_conf_file:parse("old.key = val\n"),
    AliasMap = #{"old.key" => {"new.key", CrashFun, undefined}},
    %% Should not crash; falls back to original value
    {Result, _} = run(CF, AliasMap),
    ?assertEqual({ok, "val"}, cuttlefish_conf_file:get(Result, "new.key")).

-endif.
