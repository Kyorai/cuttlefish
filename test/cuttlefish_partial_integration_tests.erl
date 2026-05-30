-module(cuttlefish_partial_integration_tests).

-include_lib("eunit/include/eunit.hrl").

include_resolves_and_expands_test() ->
    add_fixture_path(),
    Schema =
        "{include_partial, {sample_app, \"example\"},\n"
        "    [{prefix, \"x.ssl_options\"},\n"
        "     {app_prefix, \"y_app.ssl_options\"}]}.\n",
    {Translations, Mappings, _Validators} = cuttlefish_schema:strings([Schema]),
    %% 4 mappings + 1 partial_translation in the fixture.
    ?assertEqual(4, length(Mappings)),
    ?assertEqual(1, length(Translations)).

include_then_inline_merge_overrides_mapping_field_test() ->
    add_fixture_path(),
    %% Partial sets datatype = {enum, [verify_peer, verify_none]}
    %% and default = verify_none for `verify`. Override the datatype
    %% via the existing merge mechanism.
    Schema =
        "{include_partial, {sample_app, \"example\"},\n"
        "    [{prefix, \"x.ssl_options\"},\n"
        "     {app_prefix, \"y_app.ssl_options\"}]}.\n"
        "{mapping, \"x.ssl_options.verify\", \"y_app.ssl_options.verify\",\n"
        "    [merge, {datatype, atom}]}.\n",
    {_T, Mappings, _V} = cuttlefish_schema:strings([Schema]),
    Verify = find_mapping(["x", "ssl_options", "verify"], Mappings),
    ?assertEqual([atom], cuttlefish_mapping:datatype(Verify)),
    %% The default from the partial survives because merge field-merges.
    ?assertEqual(verify_none, cuttlefish_mapping:default(Verify)).

include_then_inline_translation_replaces_partial_translation_test() ->
    add_fixture_path(),
    Schema =
        "{include_partial, {sample_app, \"example\"},\n"
        "    [{prefix, \"x.ssl_options\"},\n"
        "     {app_prefix, \"y_app.ssl_options\"}]}.\n"
        "{translation, \"y_app.ssl_options.versions\",\n"
        "    fun(_Conf) -> custom_value end}.\n",
    {Translations, _M, _V} = cuttlefish_schema:strings([Schema]),
    [T] = [X || X <- Translations,
                cuttlefish_translation:mapping(X) =:= "y_app.ssl_options.versions"],
    Fun = cuttlefish_translation:func(T),
    ?assertEqual(custom_value, Fun([])).

include_same_partial_twice_with_distinct_prefixes_test() ->
    %% Models rabbit.schema's pattern of four ssl_options contexts in
    %% one file. With distinct prefixes, no key collision is possible.
    add_fixture_path(),
    Schema =
        "{include_partial, {sample_app, \"example\"},\n"
        "    [{prefix, \"primary.ssl\"},\n"
        "     {app_prefix, \"app.primary.ssl\"}]}.\n"
        "{include_partial, {sample_app, \"example\"},\n"
        "    [{prefix, \"defs.ssl\"},\n"
        "     {app_prefix, \"app.defs.ssl\"}]}.\n",
    {Translations, Mappings, _V} = cuttlefish_schema:strings([Schema]),
    %% 4 mappings per include + 1 translation per include = 10 terms.
    ?assertEqual(8, length(Mappings)),
    ?assertEqual(2, length(Translations)),
    AllMappingVars = [cuttlefish_mapping:variable(M) || M <- Mappings],
    ?assertEqual(lists:sort(AllMappingVars),
                 lists:usort(AllMappingVars)),
    AllAppKeys = [cuttlefish_translation:mapping(T) || T <- Translations],
    ?assertEqual(lists:sort(AllAppKeys), lists:usort(AllAppKeys)).

include_exclude_section_drops_mapping_and_translation_together_test() ->
    add_fixture_path(),
    Schema =
        "{include_partial, {sample_app, \"example\"},\n"
        "    [{prefix, \"x.ssl_options\"},\n"
        "     {app_prefix, \"y_app.ssl_options\"},\n"
        "     {exclude, [\"versions\"]}]}.\n",
    {Translations, Mappings, _V} = cuttlefish_schema:strings([Schema]),
    ?assertEqual([], Translations),
    Vars = [cuttlefish_mapping:variable(M) || M <- Mappings],
    ?assertNot(lists:member(["x","ssl_options","versions","$version"], Vars)).

include_exclude_exact_keeps_unrelated_test() ->
    add_fixture_path(),
    Schema =
        "{include_partial, {sample_app, \"example\"},\n"
        "    [{prefix, \"x.ssl_options\"},\n"
        "     {app_prefix, \"y_app.ssl_options\"},\n"
        "     {exclude, [\"verify\"]}]}.\n",
    {_T, Mappings, _V} = cuttlefish_schema:strings([Schema]),
    Vars = [cuttlefish_mapping:variable(M) || M <- Mappings],
    ?assertNot(lists:member(["x","ssl_options","verify"], Vars)),
    ?assert(lists:member(["x","ssl_options","cacertfile"], Vars)).

include_then_inline_addition_in_same_namespace_test() ->
    %% A consumer extends the partial by adding a sibling mapping
    %% with a partial-relative-looking variable. Both flow through.
    add_fixture_path(),
    Schema =
        "{include_partial, {sample_app, \"example\"},\n"
        "    [{prefix, \"x.ssl_options\"},\n"
        "     {app_prefix, \"y_app.ssl_options\"}]}.\n"
        "{mapping, \"x.ssl_options.cacerts.$name\", \"y_app.ssl_options.cacerts\",\n"
        "    [{datatype, string}]}.\n",
    {_T, Mappings, _V} = cuttlefish_schema:strings([Schema]),
    Vars = [cuttlefish_mapping:variable(M) || M <- Mappings],
    ?assert(lists:member(["x","ssl_options","cacerts","$name"], Vars)).

partial_source_is_prepended_to_each_emitted_mapping_doc_test() ->
    %% Every mapping emitted by a partial gets a "(from partial App:Name)"
    %% line at the head of its `doc` so `cuttlefish describe' shows the
    %% source. Original doc lines from `@doc' comments in the partial
    %% follow on subsequent lines.
    add_fixture_path(),
    Schema =
        "{include_partial, {sample_app, \"with_doc\"},\n"
        "    [{prefix, \"x.ssl\"},\n"
        "     {app_prefix, \"y.ssl\"}]}.\n",
    {_T, Mappings, _V} = cuttlefish_schema:strings([Schema]),
    Verify = find_mapping(["x", "ssl", "verify"], Mappings),
    [Provenance | RestOfDoc] = cuttlefish_mapping:doc(Verify),
    ?assertEqual("(from partial sample_app:with_doc)", Provenance),
    ?assertEqual(["Sets the verify mode.", "Multiple lines are joined."],
                 RestOfDoc).

partial_source_appears_even_when_partial_mapping_has_no_doc_test() ->
    add_fixture_path(),
    Schema =
        "{include_partial, {sample_app, \"example\"},\n"
        "    [{prefix, \"x.ssl\"},\n"
        "     {app_prefix, \"y.ssl\"}]}.\n",
    {_T, Mappings, _V} = cuttlefish_schema:strings([Schema]),
    %% `verify' in example.partial has no @doc comment; provenance
    %% becomes the sole doc line.
    Verify = find_mapping(["x", "ssl", "verify"], Mappings),
    ?assertEqual(["(from partial sample_app:example)"],
                 cuttlefish_mapping:doc(Verify)).

doc_and_see_annotations_flow_through_to_expanded_mappings_test() ->
    add_fixture_path(),
    Schema =
        "{include_partial, {sample_app, \"with_doc\"},\n"
        "    [{prefix, \"x.ssl\"},\n"
        "     {app_prefix, \"y.ssl\"}]}.\n",
    {_T, Mappings, _V} = cuttlefish_schema:strings([Schema]),
    Verify = find_mapping(["x", "ssl", "verify"], Mappings),
    %% The partial-source provenance line is prepended; partial's own
    %% @doc lines follow.
    ?assertEqual(["(from partial sample_app:with_doc)",
                  "Sets the verify mode.", "Multiple lines are joined."],
                 cuttlefish_mapping:doc(Verify)),
    ?assertEqual([["x", "ssl", "certfile"]],
                 cuttlefish_mapping:see(Verify)).

see_references_get_rewritten_through_pipeline_test() ->
    add_fixture_path(),
    Schema =
        "{include_partial, {sample_app, \"example\"},\n"
        "    [{prefix, \"x.ssl_options\"},\n"
        "     {app_prefix, \"y_app.ssl_options\"}]}.\n",
    {_T, Mappings, _V} = cuttlefish_schema:strings([Schema]),
    Cacert = find_mapping(["x", "ssl_options", "cacertfile"], Mappings),
    ?assertEqual([["x", "ssl_options", "certfile"]],
                 cuttlefish_mapping:see(Cacert)).

include_runtime_translation_uses_bound_prefix_test() ->
    %% Verifies that the bound translation actually reads
    %% prefix-rewritten conf keys at runtime.
    add_fixture_path(),
    Schema =
        "{include_partial, {sample_app, \"example\"},\n"
        "    [{prefix, \"x.ssl_options\"},\n"
        "     {app_prefix, \"y_app.ssl_options\"}]}.\n",
    {Translations, _M, _V} = cuttlefish_schema:strings([Schema]),
    [T] = Translations,
    Fun = cuttlefish_translation:func(T),
    Conf = [{["x", "ssl_options", "versions", "tlsv1.3"], 'tlsv1.3'},
            {["x", "ssl_options", "versions", "tlsv1.2"], 'tlsv1.2'}],
    Result = Fun(Conf),
    ?assertEqual(lists:sort(['tlsv1.2', 'tlsv1.3']),
                 lists:sort(Result)).

include_failure_does_not_abort_rest_of_schema_test() ->
    %% A bad include should produce an errorlist (the surrounding
    %% schema can still be parsed; reporting all errors is the
    %% existing cuttlefish_schema behaviour).
    add_fixture_path(),
    Schema =
        "{include_partial, {definitely_no_such_app_xyz, \"x\"},\n"
        "    [{prefix, \"p\"}, {app_prefix, \"a\"}]}.\n"
        "{mapping, \"unrelated\", \"unrelated\", [{datatype, atom}]}.\n",
    Result = cuttlefish_schema:strings([Schema]),
    ?assertMatch({errorlist, [_|_]}, Result),
    {errorlist, Errors} = Result,
    ?assert(lists:any(
             fun({error, {partial_app_not_loadable, _, _}}) -> true;
                (_) -> false
             end, Errors)).

full_pipeline_with_included_partial_produces_expected_app_config_test() ->
    %% End-to-end: partial mapping + bound translation flow through
    %% cuttlefish_generator:map/2 to produce a valid app.config.
    add_fixture_path(),
    Schema =
        "{include_partial, {sample_app, \"example\"},\n"
        "    [{prefix, \"x.ssl_options\"},\n"
        "     {app_prefix, \"y_app.ssl_options\"}]}.\n",
    SchemaTuple = cuttlefish_schema:strings([Schema]),
    Conf = [{["x", "ssl_options", "verify"], verify_peer},
            {["x", "ssl_options", "versions", "tlsv1.3"], 'tlsv1.3'},
            {["x", "ssl_options", "versions", "tlsv1.2"], 'tlsv1.2'}],
    Config = cuttlefish_generator:map(SchemaTuple, Conf),
    SslOpts = proplists:get_value(ssl_options,
                                  proplists:get_value(y_app, Config)),
    ?assertEqual(verify_peer, proplists:get_value(verify, SslOpts)),
    Versions = proplists:get_value(versions, SslOpts),
    ?assertEqual(lists:sort(['tlsv1.2', 'tlsv1.3']),
                 lists:sort(Versions)).

partial_with_5_tuple_validator_loads_and_runs_test() ->
    %% A partial containing a 5-tuple validator (with aliases and a
    %% deprecation hint) must pass sanitize, survive rewrite, and
    %% land in the merged validator set with its options intact.
    add_fixture_path(),
    Schema =
        "{include_partial, {sample_app, \"with_5_tuple_validator\"},\n"
        "    [{prefix, \"p\"}, {app_prefix, \"a\"}]}.\n",
    {_T, Mappings, Validators} = cuttlefish_schema:strings([Schema]),
    [V] = Validators,
    ?assertEqual("even", cuttlefish_validator:name(V)),
    ?assertEqual(["evens"], cuttlefish_validator:aliases(V)),
    ?assertMatch({"3.9.0", _}, cuttlefish_validator:deprecated(V)),
    ?assertEqual(1, length(Mappings)),
    %% The validator actually fires through the pipeline.
    ?assertMatch([{a, [{n, 4}]}],
                 cuttlefish_generator:map(
                   {_T, Mappings, Validators}, [{["p", "n"], "4"}])),
    ?assertMatch({error, validation, _},
                 cuttlefish_generator:map(
                   {_T, Mappings, Validators}, [{["p", "n"], "5"}])).

full_pipeline_applies_partial_defaults_test() ->
    %% A partial mapping with `{default, _}` contributes that default
    %% to the consumer's app.config when the conf is silent.
    add_fixture_path(),
    Schema =
        "{include_partial, {sample_app, \"example\"},\n"
        "    [{prefix, \"x.ssl_options\"},\n"
        "     {app_prefix, \"y_app.ssl_options\"}]}.\n",
    SchemaTuple = cuttlefish_schema:strings([Schema]),
    Config = cuttlefish_generator:map(SchemaTuple, []),
    SslOpts = proplists:get_value(ssl_options,
                                  proplists:get_value(y_app, Config)),
    ?assertEqual(verify_none, proplists:get_value(verify, SslOpts)).

include_and_inline_translation_coexist_in_same_schema_test() ->
    %% A schema that includes a partial AND defines its own
    %% unrelated mapping+translation: both arrive in the merged
    %% schema. The inline mapping has a default so the translation
    %% has a value to act on.
    add_fixture_path(),
    Schema =
        "{include_partial, {sample_app, \"example\"},\n"
        "    [{prefix, \"x.ssl_options\"},\n"
        "     {app_prefix, \"y_app.ssl_options\"}]}.\n"
        "{mapping, \"top\", \"y_app.top\",\n"
        "    [{datatype, atom}, {default, raw}]}.\n"
        "{translation, \"y_app.top\",\n"
        "    fun(Conf) -> {wrapped, cuttlefish:conf_get(\"top\", Conf)} end}.\n",
    SchemaTuple = cuttlefish_schema:strings([Schema]),
    Config = cuttlefish_generator:map(SchemaTuple, []),
    YApp = proplists:get_value(y_app, Config),
    ?assertEqual({wrapped, raw}, proplists:get_value(top, YApp)).

disable_with_end_to_end_disables_via_atom_test() ->
    add_fixture_path(),
    Schema =
        "{include_partial, {sample_app, \"example\"},\n"
        "    [{prefix, \"x.ssl_options\"},\n"
        "     {app_prefix, \"y_app.ssl_options\"},\n"
        "     {disable_with, none}]}.\n",
    SchemaTuple = cuttlefish_schema:strings([Schema]),
    Conf = [{["x", "ssl_options"], none}],
    Config = cuttlefish_generator:map(SchemaTuple, Conf),
    YApp = proplists:get_value(y_app, Config),
    ?assertEqual([], proplists:get_value(ssl_options, YApp)).

overrides_end_to_end_replaces_partial_field_test() ->
    add_fixture_path(),
    %% The fixture's `verify` mapping has default `verify_none`. The
    %% override changes it to `verify_peer`.
    Schema =
        "{include_partial, {sample_app, \"example\"},\n"
        "    [{prefix, \"x.ssl_options\"},\n"
        "     {app_prefix, \"y_app.ssl_options\"},\n"
        "     {overrides, [{\"verify\", {default, verify_peer}}]}]}.\n",
    SchemaTuple = cuttlefish_schema:strings([Schema]),
    Config = cuttlefish_generator:map(SchemaTuple, []),
    SslOpts = proplists:get_value(ssl_options,
                                  proplists:get_value(y_app, Config)),
    ?assertEqual(verify_peer, proplists:get_value(verify, SslOpts)).

exclude_typo_surfaces_through_strings_pipeline_test() ->
    %% The pre-flight `exclude' name check fires inside the schema
    %% parser path and surfaces as a normal cuttlefish errorlist.
    add_fixture_path(),
    Schema =
        "{include_partial, {sample_app, \"example\"},\n"
        "    [{prefix, \"x.ssl_options\"},\n"
        "     {app_prefix, \"y_app.ssl_options\"},\n"
        "     {exclude, [\"definitely_not_there\"]}]}.\n",
    Result = cuttlefish_schema:strings([Schema]),
    ?assertMatch({errorlist, [_|_]}, Result),
    {errorlist, Errors} = Result,
    ?assert(lists:any(
             fun({error, {partial_exclude_unmatched, _, _,
                          ["definitely_not_there"]}}) -> true;
                (_) -> false
             end, Errors)).

overrides_typo_surfaces_through_strings_pipeline_test() ->
    add_fixture_path(),
    Schema =
        "{include_partial, {sample_app, \"example\"},\n"
        "    [{prefix, \"x.ssl_options\"},\n"
        "     {app_prefix, \"y_app.ssl_options\"},\n"
        "     {overrides, [{\"certfle\", {validators, [\"v\"]}}]}]}.\n",
    Result = cuttlefish_schema:strings([Schema]),
    ?assertMatch({errorlist, [_|_]}, Result),
    {errorlist, Errors} = Result,
    ?assert(lists:any(
             fun({error, {partial_overrides_unmatched, _, _,
                          ["certfle"]}}) -> true;
                (_) -> false
             end, Errors)).

disable_with_translation_is_dropped_when_parent_absent_test() ->
    %% When the user sets a sub-key but not the parent disable atom,
    %% the guard translation is correctly dropped (no spurious
    %% `cuttlefish:invalid' error). The sub-key still produces its
    %% nested value.
    add_fixture_path(),
    Schema =
        "{include_partial, {sample_app, \"example\"},\n"
        "    [{prefix, \"x.ssl_options\"},\n"
        "     {app_prefix, \"y_app.ssl_options\"},\n"
        "     {disable_with, none}]}.\n",
    SchemaTuple = cuttlefish_schema:strings([Schema]),
    Conf = [{["x", "ssl_options", "verify"], verify_peer}],
    Config = cuttlefish_generator:map(SchemaTuple, Conf),
    SslOpts = proplists:get_value(ssl_options,
                                  proplists:get_value(y_app, Config)),
    ?assertEqual(verify_peer, proplists:get_value(verify, SslOpts)),
    %% No spurious empty list from the guard.
    ?assertEqual([{verify, verify_peer}], SslOpts).

malformed_include_directive_produces_meaningful_error_test() ->
    %% A user typo that doesn't follow the {App, "name"} shape
    %% surfaces as a partial_bad_directive error, not the generic
    %% "Unknown parse return".
    Schema = "{include_partial, \"rabbit:ssl_options\", []}.\n",
    Result = cuttlefish_schema:strings([Schema]),
    ?assertMatch({errorlist, [_|_]}, Result),
    {errorlist, Errors} = Result,
    ?assert(lists:any(
             fun({error, {partial_bad_directive, _}}) -> true;
                (_) -> false
             end, Errors)).

schema_starting_with_include_partial_is_recognised_test() ->
    %% looks_like_schema accepts include_partial as a top-level tag.
    add_fixture_path(),
    Dir = unique_tmp_dir(),
    Path = filename:join(Dir, "include_first.schema"),
    Bytes = <<"{include_partial, {sample_app, \"empty\"},\n"
              "    [{prefix, \"x\"}, {app_prefix, \"y\"}]}.\n">>,
    ok = file:write_file(Path, Bytes),
    try
        ?assertMatch({_, _, _}, cuttlefish_schema:files([Path]))
    after
        file:delete(Path),
        cleanup_dir(Dir)
    end.

partial_extension_is_not_picked_up_by_list_schemas_test() ->
    %% Defence-in-depth: list_schemas must keep filtering on .schema,
    %% so a partial dropped alongside a schema is never auto-loaded.
    Dir = unique_tmp_dir(),
    ok = file:write_file(filename:join(Dir, "a.schema"),
                         <<"{mapping, \"a\", \"a\", []}.\n">>),
    ok = file:write_file(filename:join(Dir, "b.partial"),
                         <<"{mapping, \"b\", \"b\", []}.\n">>),
    try
        Files = cuttlefish_schema:list_schemas(Dir),
        Names = [filename:basename(F) || F <- Files],
        ?assertEqual(["a.schema"], Names)
    after
        cleanup_dir(Dir)
    end.

%% --- helpers ------------------------------------------------------

add_fixture_path() ->
    Ebin = fixture_ebin(),
    ok = ensure_sample_app_file(Ebin),
    true = filelib:is_dir(Ebin) orelse
        erlang:error({fixture_ebin_missing, Ebin}),
    code:add_pathz(Ebin),
    ok.

%% Write `sample_app.app` if absent; rebar3 .gitignore excludes
%% `ebin/`, so the fixture .app file isn't carried in version control.
ensure_sample_app_file(Ebin) ->
    ok = filelib:ensure_dir(filename:join(Ebin, "marker")),
    AppFile = filename:join(Ebin, "sample_app.app"),
    case filelib:is_regular(AppFile) of
        true  -> ok;
        false ->
            App = "{application, sample_app,\n"
                  " [{description, \"Test fixture for cuttlefish partials\"},\n"
                  "  {vsn, \"0.0.1\"},\n"
                  "  {modules, []},\n"
                  "  {registered, []},\n"
                  "  {applications, [kernel, stdlib]},\n"
                  "  {env, []}]}.\n",
            ok = file:write_file(AppFile, App)
    end.

fixture_ebin() ->
    %% Resolve from cuttlefish's lib_dir (always set during tests) up
    %% 4 levels to the project root, then into the source-tree fixture.
    %% Avoids depending on rebar3 copying test/fixtures into _build.
    LibDir = code:lib_dir(cuttlefish),
    Project = filename:absname(
                filename:join([LibDir, "..", "..", "..", ".."])),
    filename:join([Project, "test", "fixtures",
                   "sample_app", "ebin"]).

find_mapping(Var, Mappings) ->
    case [M || M <- Mappings, cuttlefish_mapping:variable(M) =:= Var] of
        [M] -> M;
        [] -> erlang:error({mapping_not_found, Var});
        Many -> erlang:error({multiple_mappings_for, Var, length(Many)})
    end.

unique_tmp_dir() ->
    Path = filename:join(cuttlefish_paths:tmp_base(),
                         "cuttlefish_partial_integration_"
                         ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = file:make_dir(Path),
    Path.

cleanup_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:foreach(
              fun(F) -> file:delete(filename:join(Dir, F)) end,
              Files);
        _ -> ok
    end,
    file:del_dir(Dir).
