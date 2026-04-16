%% -------------------------------------------------------------------
%%
%% End-to-end tests for the cuttlefish migrate command.
%% Tests the full pipeline: schema (with aliases) + conf file ->
%% migration -> verify output.
%%
%% -------------------------------------------------------------------
-module(cuttlefish_migration_tests).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Helpers
%% ===================================================================

schema(SchemaStrings) ->
    cuttlefish_schema:strings(SchemaStrings).

migrate(ConfString, SchemaStrings) ->
    {_, Mappings, _} = schema(SchemaStrings),
    CF = cuttlefish_conf_file:parse(ConfString),
    AliasMap = cuttlefish_migrate:build_alias_map(Mappings),
    cuttlefish_migrate:run(CF, AliasMap).

%% ===================================================================
%% Simple rename tests
%% ===================================================================

simple_rename_test() ->
    S = ["{mapping, \"new.key\", \"app.setting\", [\n"
         "  {datatype, integer},\n"
         "  {default, 10},\n"
         "  {aliases, [\"old.key\"]}\n"
         "]}.\n"],
    {Result, Changes} = migrate("old.key = 42\n", S),
    ?assertEqual({ok, "42"}, cuttlefish_conf_file:get(Result, "new.key")),
    ?assertEqual(undefined, cuttlefish_conf_file:get(Result, "old.key")),
    ?assertMatch([{rename, "old.key", "new.key"}], Changes).

multiple_aliases_first_wins_test() ->
    S = ["{mapping, \"new.key\", \"app.setting\", [\n"
         "  {datatype, integer},\n"
         "  {aliases, [\"old.key\", \"oldest.key\"]}\n"
         "]}.\n"],
    {Result, Changes} = migrate("oldest.key = 99\n", S),
    ?assertEqual({ok, "99"}, cuttlefish_conf_file:get(Result, "new.key")),
    ?assertEqual(1, length(Changes)).

canonical_already_set_test() ->
    S = ["{mapping, \"new.key\", \"app.setting\", [\n"
         "  {datatype, integer},\n"
         "  {aliases, [\"old.key\"]}\n"
         "]}.\n"],
    {Result, Changes} = migrate("new.key = 100\nold.key = 42\n", S),
    ?assertEqual({ok, "100"}, cuttlefish_conf_file:get(Result, "new.key")),
    ?assertEqual(undefined, cuttlefish_conf_file:get(Result, "old.key")),
    ?assertMatch([{conflict, "old.key", "new.key"}], Changes),
    Output = cuttlefish_conf_file:to_string(Result),
    ?assertMatch({match, _}, re:run(Output, "# old\\.key = 42")).

no_aliases_no_changes_test() ->
    S = ["{mapping, \"a.b\", \"app.x\", [\n"
         "  {datatype, integer}\n"
         "]}.\n"],
    {_, Changes} = migrate("a.b = 1\n", S),
    ?assertEqual([], Changes).

no_matching_keys_test() ->
    S = ["{mapping, \"new.key\", \"app.setting\", [\n"
         "  {datatype, integer},\n"
         "  {aliases, [\"old.key\"]}\n"
         "]}.\n"],
    {_, Changes} = migrate("unrelated = val\n", S),
    ?assertEqual([], Changes).

%% ===================================================================
%% Migration function tests
%% ===================================================================

value_migration_test() ->
    S = ["{mapping, \"new.level\", \"app.log_level\", [\n"
         "  {datatype, {enum, [debug, info, warning, error]}},\n"
         "  {aliases, [\"old.level\"]},\n"
         "  {migration, fun(\"warn\") -> \"warning\"; (V) -> V end}\n"
         "]}.\n"],
    {Result, Changes} = migrate("old.level = warn\n", S),
    ?assertEqual({ok, "warning"}, cuttlefish_conf_file:get(Result, "new.level")),
    ?assertMatch([{rename, "old.level", "new.level", "warn", "warning"}], Changes).

value_migration_identity_test() ->
    S = ["{mapping, \"new.key\", \"app.x\", [\n"
         "  {datatype, string},\n"
         "  {aliases, [\"old.key\"]},\n"
         "  {migration, fun(V) -> V end}\n"
         "]}.\n"],
    {Result, Changes} = migrate("old.key = same\n", S),
    ?assertEqual({ok, "same"}, cuttlefish_conf_file:get(Result, "new.key")),
    ?assertMatch([{rename, "old.key", "new.key"}], Changes).

%% ===================================================================
%% Format preservation tests
%% ===================================================================

preserves_comments_test() ->
    S = ["{mapping, \"new.key\", \"app.x\", [\n"
         "  {datatype, integer},\n"
         "  {aliases, [\"old.key\"]}\n"
         "]}.\n"],
    Input = "# Important comment\n\nold.key = 42\n# Trailing\n",
    {Result, _} = migrate(Input, S),
    Output = cuttlefish_conf_file:to_string(Result),
    ?assertMatch({match, _}, re:run(Output, "# Important comment")),
    ?assertMatch({match, _}, re:run(Output, "# Trailing")).

preserves_unrelated_settings_test() ->
    S = ["{mapping, \"new.key\", \"app.x\", [\n"
         "  {datatype, integer},\n"
         "  {aliases, [\"old.key\"]}\n"
         "]}.\n"],
    Input = "unrelated = hello\nold.key = 42\nother = world\n",
    {Result, _} = migrate(Input, S),
    ?assertEqual({ok, "hello"}, cuttlefish_conf_file:get(Result, "unrelated")),
    ?assertEqual({ok, "world"}, cuttlefish_conf_file:get(Result, "other")).

adds_migration_comment_test() ->
    S = ["{mapping, \"new.key\", \"app.x\", [\n"
         "  {datatype, integer},\n"
         "  {aliases, [\"old.key\"]}\n"
         "]}.\n"],
    {Result, _} = migrate("old.key = 42\n", S),
    Output = cuttlefish_conf_file:to_string(Result),
    ?assertMatch({match, _}, re:run(Output, "# Migrated: old\\.key -> new\\.key")).

%% ===================================================================
%% Idempotency tests
%% ===================================================================

idempotent_test() ->
    S = ["{mapping, \"new.key\", \"app.x\", [\n"
         "  {datatype, integer},\n"
         "  {aliases, [\"old.key\"]}\n"
         "]}.\n"],
    {_, Mappings, _} = schema(S),
    AliasMap = cuttlefish_migrate:build_alias_map(Mappings),

    CF0 = cuttlefish_conf_file:parse("old.key = 42\n"),
    {CF1, Changes1} = cuttlefish_migrate:run(CF0, AliasMap),
    ?assertEqual(1, length(Changes1)),

    {CF2, Changes2} = cuttlefish_migrate:run(CF1, AliasMap),
    ?assertEqual(0, length(Changes2)),
    ?assertEqual(cuttlefish_conf_file:to_string(CF1),
                 cuttlefish_conf_file:to_string(CF2)).

%% ===================================================================
%% Multiple independent mappings
%% ===================================================================

multiple_independent_mappings_test() ->
    S = ["{mapping, \"new.a\", \"app.x\", [\n"
         "  {datatype, integer},\n"
         "  {aliases, [\"old.a\"]}\n"
         "]}.\n"
         "{mapping, \"new.b\", \"app.y\", [\n"
         "  {datatype, string},\n"
         "  {aliases, [\"old.b\"]}\n"
         "]}.\n"],
    {Result, Changes} = migrate("old.a = 1\nold.b = hello\n", S),
    ?assertEqual(2, length(Changes)),
    ?assertEqual({ok, "1"}, cuttlefish_conf_file:get(Result, "new.a")),
    ?assertEqual({ok, "hello"}, cuttlefish_conf_file:get(Result, "new.b")).

%% ===================================================================
%% Alias with custom message
%% ===================================================================

alias_with_message_test() ->
    S = ["{mapping, \"new.key\", \"app.x\", [\n"
         "  {datatype, integer},\n"
         "  {aliases, [{\"old.key\", \"renamed in 4.0\"}]}\n"
         "]}.\n"],
    {Result, Changes} = migrate("old.key = 42\n", S),
    ?assertEqual({ok, "42"}, cuttlefish_conf_file:get(Result, "new.key")),
    ?assertMatch([{rename, "old.key", "new.key"}], Changes).

%% ===================================================================
%% build_alias_map with migration fun from schema
%% ===================================================================

alias_map_includes_migration_fun_test() ->
    S = ["{mapping, \"new.key\", \"app.x\", [\n"
         "  {datatype, string},\n"
         "  {aliases, [\"old.key\"]},\n"
         "  {migration, fun(V) -> V end}\n"
         "]}.\n"],
    {_, Mappings, _} = schema(S),
    Map = cuttlefish_migrate:build_alias_map(Mappings),
    ?assertEqual(1, maps:size(Map)),
    {ok, {Canonical, MigFun, _Msg}} = maps:find("old.key", Map),
    ?assertEqual("new.key", Canonical),
    ?assert(is_function(MigFun, 1)).

%% ===================================================================
%% File I/O round-trip
%% ===================================================================

file_roundtrip_test() ->
    S = ["{mapping, \"new.key\", \"app.x\", [\n"
         "  {datatype, integer},\n"
         "  {aliases, [\"old.key\"]}\n"
         "]}.\n"],
    {_, Mappings, _} = schema(S),
    AliasMap = cuttlefish_migrate:build_alias_map(Mappings),

    TmpDir = filename:join(["/tmp", "cuttlefish_migrate_test_" ++
                            integer_to_list(erlang:unique_integer([positive]))]),
    ok = filelib:ensure_dir(filename:join(TmpDir, "dummy")),
    InputFile = filename:join(TmpDir, "input.conf"),
    OutputFile = filename:join(TmpDir, "output.conf"),

    ok = file:write_file(InputFile, "# Header\nold.key = 42\n"),
    {ok, CF} = cuttlefish_conf_file:load(InputFile),
    {Migrated, _} = cuttlefish_migrate:run(CF, AliasMap),
    ok = cuttlefish_conf_file:save(Migrated, OutputFile),

    {ok, Reloaded} = cuttlefish_conf_file:load(OutputFile),
    ?assertEqual({ok, "42"}, cuttlefish_conf_file:get(Reloaded, "new.key")),
    ?assertEqual(undefined, cuttlefish_conf_file:get(Reloaded, "old.key")),

    %% Cleanup
    file:delete(InputFile),
    file:delete(OutputFile),
    file:del_dir(TmpDir).

%% ===================================================================
%% Mixed scenario: some keys need migration, some don't
%% ===================================================================

mixed_scenario_test() ->
    S = ["{mapping, \"new.renamed\", \"app.x\", [\n"
         "  {datatype, integer},\n"
         "  {aliases, [\"old.renamed\"]}\n"
         "]}.\n"
         "{mapping, \"stays.same\", \"app.y\", [\n"
         "  {datatype, string}\n"
         "]}.\n"
         "{mapping, \"also.new\", \"app.z\", [\n"
         "  {datatype, integer},\n"
         "  {aliases, [\"also.old\"]}\n"
         "]}.\n"],
    Input = "old.renamed = 1\nstays.same = hello\nalso.old = 2\n",
    {Result, Changes} = migrate(Input, S),
    ?assertEqual(2, length(Changes)),
    ?assertEqual({ok, "1"}, cuttlefish_conf_file:get(Result, "new.renamed")),
    ?assertEqual({ok, "hello"}, cuttlefish_conf_file:get(Result, "stays.same")),
    ?assertEqual({ok, "2"}, cuttlefish_conf_file:get(Result, "also.new")).

%% ===================================================================
%% Conflict with commented-out alias preserves canonical value
%% ===================================================================

conflict_output_format_test() ->
    S = ["{mapping, \"new.key\", \"app.x\", [\n"
         "  {datatype, integer},\n"
         "  {aliases, [\"old.key\"]}\n"
         "]}.\n"],
    Input = "new.key = 100\nold.key = 42\n",
    {Result, _} = migrate(Input, S),
    Output = cuttlefish_conf_file:to_string(Result),
    ?assertEqual({ok, "100"}, cuttlefish_conf_file:get(Result, "new.key")),
    ?assertMatch({match, _}, re:run(Output, "# Conflict: old\\.key is superseded by new\\.key")),
    ?assertMatch({match, _}, re:run(Output, "# old\\.key = 42")).
