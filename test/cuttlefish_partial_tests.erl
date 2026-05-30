-module(cuttlefish_partial_tests).

-include_lib("eunit/include/eunit.hrl").

-define(XLATE(X), lists:flatten(cuttlefish_error:xlate(X))).

-define(BASE_OPTS,
        [{prefix, "x.ssl_options"},
         {app_prefix, "y_app.ssl_options"}]).

%% --- rewrite/2 -----------------------------------------------------

mapping_conf_and_app_keys_get_prefixed_test() ->
    Term = {mapping, "verify", "verify", [{datatype, atom}]},
    {ok, [Out]} = cuttlefish_partial:rewrite([Term], ?BASE_OPTS),
    ?assertMatch({mapping, "x.ssl_options.verify",
                          "y_app.ssl_options.verify",
                          [{datatype, atom}]},
                 Out).

mapping_with_fuzzy_segment_is_preserved_test() ->
    Term = {mapping, "versions.$version", "versions", [{datatype, atom}]},
    {ok, [Out]} = cuttlefish_partial:rewrite([Term], ?BASE_OPTS),
    ?assertEqual({mapping, "x.ssl_options.versions.$version",
                          "y_app.ssl_options.versions",
                          [{datatype, atom}]},
                 Out).

mapping_opts_pass_through_unchanged_test() ->
    Opts = [{datatype, integer}, {default, 5},
            {validators, ["v"]}, {hidden, true},
            {level, advanced}, {commented, 10},
            {include_default, "name"},
            {aliases, ["legacy.absolute.key"]}],
    Term = {mapping, "n", "n", Opts},
    {ok, [{mapping, _, _, Out}]} =
        cuttlefish_partial:rewrite([Term], ?BASE_OPTS),
    ?assertEqual(Opts, Out).

aliases_are_not_prefix_rewritten_test() ->
    Opts = [{aliases, ["legacy.absolute.key"]}],
    Term = {mapping, "n", "n", Opts},
    {ok, [{mapping, _, _, Out}]} =
        cuttlefish_partial:rewrite([Term], ?BASE_OPTS),
    ?assertEqual([["legacy.absolute.key"]],
                 proplists:get_all_values(aliases, Out)).

see_bare_entries_get_prefix_dotted_entries_do_not_test() ->
    %% Both kinds get tokenized so downstream code (which expects
    %% `[variable()]`) treats them uniformly.
    Term = {mapping, "cacertfile", "cacertfile",
            [{see, ["certfile", "absolute.elsewhere"]}]},
    {ok, [{mapping, _, _, Out}]} =
        cuttlefish_partial:rewrite([Term], ?BASE_OPTS),
    ?assertEqual([{see, [["x", "ssl_options", "certfile"],
                         ["absolute", "elsewhere"]]}],
                 Out).

validator_passes_through_unchanged_test() ->
    Fun = fun(_) -> true end,
    Term = {validator, "pem_file", "must be a PEM file", Fun},
    {ok, [Out]} = cuttlefish_partial:rewrite([Term], ?BASE_OPTS),
    ?assertEqual(Term, Out).

validator_5_tuple_passes_through_unchanged_test() ->
    %% A validator declared with an options proplist (aliases,
    %% deprecation hint) survives partial rewriting unchanged.
    Fun = fun(_) -> true end,
    Term = {validator, "pem_file", "must be a PEM file", Fun,
            [{aliases, ["pem"]},
             {deprecated, "3.9.0", "use the {datatype, file} form"}]},
    {ok, [Out]} = cuttlefish_partial:rewrite([Term], ?BASE_OPTS),
    ?assertEqual(Term, Out).

partial_translation_emits_arity1_closure_with_bound_prefixes_test() ->
    Term = {partial_translation, "versions",
            fun(C, P, A) -> {C, P, A} end},
    {ok, [Out]} = cuttlefish_partial:rewrite([Term], ?BASE_OPTS),
    ?assertMatch({translation, "y_app.ssl_options.versions", _}, Out),
    {translation, _, Bound} = Out,
    ?assertEqual({arity, 1}, erlang:fun_info(Bound, arity)),
    ?assertEqual({fake_conf, "x.ssl_options", "y_app.ssl_options"},
                 Bound(fake_conf)).

partial_translation_with_bad_arity_returns_error_test() ->
    Bad1 = {partial_translation, "v", fun(_) -> ok end},
    Bad2 = {partial_translation, "v", fun(_, _) -> ok end},
    Bad4 = {partial_translation, "v", fun(_, _, _, _) -> ok end},
    ?assertMatch({error, {partial_translation_bad_arity, "v", 1}},
                 cuttlefish_partial:rewrite([Bad1], ?BASE_OPTS)),
    ?assertMatch({error, {partial_translation_bad_arity, "v", 2}},
                 cuttlefish_partial:rewrite([Bad2], ?BASE_OPTS)),
    ?assertMatch({error, {partial_translation_bad_arity, "v", 4}},
                 cuttlefish_partial:rewrite([Bad4], ?BASE_OPTS)).

%% --- exclude semantics --------------------------------------------

exclude_drops_an_exact_bare_name_test() ->
    Terms = [{mapping, "verify", "verify", []},
             {mapping, "cacertfile", "cacertfile", []}],
    Opts = ?BASE_OPTS ++ [{exclude, ["verify"]}],
    {ok, Out} = cuttlefish_partial:rewrite(Terms, Opts),
    ?assertEqual(1, length(Out)),
    [{mapping, "x.ssl_options.cacertfile", _, _}] = Out.

exclude_section_drops_a_mapping_family_test() ->
    Terms = [{mapping, "versions.$version", "versions", []},
             {mapping, "verify", "verify", []}],
    Opts = ?BASE_OPTS ++ [{exclude, ["versions"]}],
    {ok, Out} = cuttlefish_partial:rewrite(Terms, Opts),
    ?assertEqual(1, length(Out)),
    [{mapping, "x.ssl_options.verify", _, _}] = Out.

exclude_section_drops_matching_translation_test() ->
    Terms = [{partial_translation, "versions",
              fun(_, _, _) -> [] end},
             {partial_translation, "ciphers",
              fun(_, _, _) -> [] end}],
    Opts = ?BASE_OPTS ++ [{exclude, ["versions"]}],
    {ok, Out} = cuttlefish_partial:rewrite(Terms, Opts),
    ?assertEqual(1, length(Out)),
    [{translation, "y_app.ssl_options.ciphers", _}] = Out.

exclude_first_segment_match_is_exact_not_substring_test() ->
    %% "versions" must not drop "versions_extra".
    Terms = [{mapping, "versions_extra", "versions_extra", []},
             {mapping, "versions.$v", "versions", []}],
    Opts = ?BASE_OPTS ++ [{exclude, ["versions"]}],
    {ok, Out} = cuttlefish_partial:rewrite(Terms, Opts),
    ?assertEqual(1, length(Out)),
    [{mapping, "x.ssl_options.versions_extra", _, _}] = Out.

exclude_empty_list_is_a_noop_test() ->
    Terms = [{mapping, "verify", "verify", []}],
    Opts = ?BASE_OPTS ++ [{exclude, []}],
    {ok, [_]} = cuttlefish_partial:rewrite(Terms, Opts).

%% --- overrides ----------------------------------------------------

overrides_replace_a_mapping_field_test() ->
    Terms = [{mapping, "cacertfile", "cacertfile",
              [{datatype, string}, {validators, ["pem_file"]}]}],
    Opts = ?BASE_OPTS ++ [{overrides, [{"cacertfile",
                                         {validators, ["file_accessible"]}}]}],
    {ok, [{mapping, _, _, OutOpts}]} =
        cuttlefish_partial:rewrite(Terms, Opts),
    %% Override-supplied {validators, _} wins over the partial's default.
    ?assertEqual(["file_accessible"], proplists:get_value(validators, OutOpts)),
    %% Other opts from the partial survive unchanged.
    ?assertEqual(string, proplists:get_value(datatype, OutOpts)).

overrides_only_touch_named_mappings_test() ->
    Terms = [{mapping, "cacertfile", "cacertfile", [{validators, ["pem_file"]}]},
             {mapping, "certfile", "certfile",   [{validators, ["pem_file"]}]}],
    Opts = ?BASE_OPTS ++ [{overrides, [{"cacertfile",
                                         {validators, ["file_accessible"]}}]}],
    {ok, [{mapping, _, _, O1}, {mapping, _, _, O2}]} =
        cuttlefish_partial:rewrite(Terms, Opts),
    ?assertEqual(["file_accessible"], proplists:get_value(validators, O1)),
    ?assertEqual(["pem_file"], proplists:get_value(validators, O2)).

overrides_can_add_a_new_opt_test() ->
    Terms = [{mapping, "verify", "verify", [{datatype, atom}]}],
    Opts = ?BASE_OPTS ++ [{overrides, [{"verify", {default, verify_peer}}]}],
    {ok, [{mapping, _, _, OutOpts}]} =
        cuttlefish_partial:rewrite(Terms, Opts),
    ?assertEqual(verify_peer, proplists:get_value(default, OutOpts)).

overrides_with_bad_shape_is_rejected_test() ->
    Bad = ?BASE_OPTS ++ [{overrides, [{"verify", not_a_tuple}]}],
    ?assertMatch({error, {partial_unknown_include_opt, {overrides, _}}},
                 cuttlefish_partial:load(sample_app, "example", Bad)).

%% --- disable_with -------------------------------------------------

disable_with_adds_guard_mapping_and_translation_test() ->
    Terms = [{mapping, "verify", "verify", [{datatype, atom}]}],
    Opts = ?BASE_OPTS ++ [{disable_with, none}],
    {ok, Out} = cuttlefish_partial:rewrite(Terms, Opts),
    ?assertEqual(3, length(Out)),
    %% Partial mapping survives.
    ?assert(lists:any(
        fun({mapping, "x.ssl_options.verify", _, _}) -> true;
           (_) -> false
        end, Out)),
    %% Guard mapping uses the bare prefix as its conf key.
    [Guard] = [M || {mapping, "x.ssl_options", _, _} = M <- Out],
    ?assertMatch({mapping, _, "y_app.ssl_options",
                  [{datatype, {enum, [none]}}]}, Guard),
    %% Guard translation accepts the configured atom and produces [].
    [{translation, _, GuardFun}] = [T || {translation, _, _} = T <- Out],
    ?assertEqual([], GuardFun([{["x", "ssl_options"], none}])).

disable_with_rejects_other_values_via_translation_test() ->
    Opts = ?BASE_OPTS ++ [{disable_with, none}],
    {ok, Out} = cuttlefish_partial:rewrite([], Opts),
    [{translation, _, F}] = [T || {translation, _, _} = T <- Out],
    %% A non-matching value triggers `cuttlefish:invalid`, which throws.
    ?assertThrow({invalid, _},
                 F([{["x", "ssl_options"], something_else}])).

disable_with_non_atom_is_rejected_test() ->
    Bad = ?BASE_OPTS ++ [{disable_with, "string-not-atom"}],
    ?assertMatch({error, {partial_unknown_include_opt,
                          {disable_with, "string-not-atom"}}},
                 cuttlefish_partial:load(sample_app, "example", Bad)).

disable_with_guard_returns_unset_when_parent_absent_test() ->
    %% Defence-in-depth: cuttlefish_generator drops translations whose
    %% target has no contributing mapping value today, so this branch
    %% is unreachable through `cuttlefish_generator:map/2'. If that
    %% behaviour ever changes the translation still degrades gracefully.
    Opts = ?BASE_OPTS ++ [{disable_with, none}],
    {ok, Out} = cuttlefish_partial:rewrite([], Opts),
    [{translation, _, F}] = [T || {translation, _, _} = T <- Out],
    ?assertThrow(unset, F([])).

disable_with_combined_with_overrides_test() ->
    %% disable_with appends the guard after the rewritten body, so
    %% overrides on the body and the guard coexist without
    %% interfering.
    Terms = [{mapping, "verify", "verify",
              [{datatype, atom}, {default, verify_none}]}],
    Opts = ?BASE_OPTS
        ++ [{overrides, [{"verify", {default, verify_peer}}]},
            {disable_with, none}],
    {ok, Out} = cuttlefish_partial:rewrite(Terms, Opts),
    %% 1 partial mapping + 1 guard mapping + 1 guard translation.
    ?assertEqual(3, length(Out)),
    [{mapping, "x.ssl_options.verify", _, OutOpts}] =
        [M || {mapping, "x.ssl_options.verify", _, _} = M <- Out],
    ?assertEqual(verify_peer, proplists:get_value(default, OutOpts)).

%% --- include-opt validation ---------------------------------------

missing_prefix_is_rejected_test() ->
    Bad = [{app_prefix, "y"}],
    ?assertMatch({error, {partial_missing_prefix, sample_app, "example"}},
                 cuttlefish_partial:load(sample_app, "example", Bad)).

missing_app_prefix_is_rejected_test() ->
    Bad = [{prefix, "x"}],
    ?assertMatch({error, {partial_missing_app_prefix, sample_app, "example"}},
                 cuttlefish_partial:load(sample_app, "example", Bad)).

empty_prefix_is_rejected_test() ->
    Bad = [{prefix, ""}, {app_prefix, "y"}],
    ?assertMatch({error, {partial_empty_prefix, sample_app, "example"}},
                 cuttlefish_partial:load(sample_app, "example", Bad)).

empty_app_prefix_is_rejected_test() ->
    Bad = [{prefix, "x"}, {app_prefix, ""}],
    ?assertMatch({error, {partial_empty_prefix, sample_app, "example"}},
                 cuttlefish_partial:load(sample_app, "example", Bad)).

unknown_include_opt_is_rejected_test() ->
    Bad = [{predix, "x"}, {app_prefix, "y"}],
    ?assertMatch({error, {partial_unknown_include_opt, {predix, "x"}}},
                 cuttlefish_partial:load(sample_app, "example", Bad)).

opts_must_be_a_list_test() ->
    ?assertMatch({error, {partial_unknown_include_opt, not_a_list}},
                 cuttlefish_partial:load(sample_app, "example", not_a_list)).

%% --- loader (filesystem + app) ------------------------------------

load_unknown_app_returns_error_test() ->
    Result = cuttlefish_partial:load(definitely_no_such_app_xyz, "example",
                                      ?BASE_OPTS),
    ?assertMatch({error, {partial_app_not_loadable, definitely_no_such_app_xyz, _}},
                 Result).

load_missing_partial_returns_file_not_found_test() ->
    add_fixture_path(),
    Result = cuttlefish_partial:load(sample_app, "no_such_partial",
                                      ?BASE_OPTS),
    ?assertMatch({error, {partial_file_not_found, sample_app,
                          "no_such_partial", _}}, Result).

load_partial_with_plain_translation_returns_helpful_error_test() ->
    add_fixture_path(),
    Result = cuttlefish_partial:load(sample_app, "with_translation",
                                      ?BASE_OPTS),
    ?assertEqual({error, {partial_unsupported_term, translation}}, Result).

load_partial_with_nested_include_is_rejected_test() ->
    add_fixture_path(),
    Result = cuttlefish_partial:load(sample_app, "with_nested_include",
                                      ?BASE_OPTS),
    ?assertEqual({error, {partial_include_in_partial, sample_app,
                          "with_nested_include"}}, Result).

load_partial_with_bad_arity_is_rejected_test() ->
    add_fixture_path(),
    Result = cuttlefish_partial:load(sample_app, "with_bad_arity",
                                      ?BASE_OPTS),
    ?assertEqual({error, {partial_translation_bad_arity, "versions", 2}},
                 Result).

load_partial_with_unsupported_term_is_rejected_test() ->
    add_fixture_path(),
    Result = cuttlefish_partial:load(sample_app, "with_unsupported",
                                      ?BASE_OPTS),
    ?assertEqual({error, {partial_unsupported_term, config}}, Result).

%% --- exclude/overrides name validation ---------------------------

exclude_with_typo_is_rejected_test() ->
    %% A misspelled exclude name (`versiosns`) currently no-ops
    %% silently without validation. The pre-flight catches it.
    add_fixture_path(),
    Bad = ?BASE_OPTS ++ [{exclude, ["versiosns"]}],
    Result = cuttlefish_partial:load(sample_app, "example", Bad),
    ?assertMatch({error, {partial_exclude_unmatched, sample_app, "example",
                          ["versiosns"]}}, Result).

exclude_lists_all_unmatched_names_at_once_test() ->
    add_fixture_path(),
    Bad = ?BASE_OPTS ++ [{exclude, ["typo1", "verify", "typo2"]}],
    Result = cuttlefish_partial:load(sample_app, "example", Bad),
    %% Valid `verify` survives; both typos surface in one error.
    ?assertMatch({error, {partial_exclude_unmatched, sample_app, "example",
                          ["typo1", "typo2"]}}, Result).

exclude_section_match_counts_as_matched_test() ->
    %% `versions` (the section name) matches the `versions.$version`
    %% mapping via first-segment match, so validation accepts it
    %% even though there is no exact-named mapping called `versions`.
    add_fixture_path(),
    Opts = ?BASE_OPTS ++ [{exclude, ["versions"]}],
    {ok, _} = cuttlefish_partial:load(sample_app, "example", Opts).

overrides_with_typo_is_rejected_test() ->
    add_fixture_path(),
    Bad = ?BASE_OPTS ++ [{overrides, [{"certfle",
                                        {validators, ["file_accessible"]}}]}],
    Result = cuttlefish_partial:load(sample_app, "example", Bad),
    ?assertMatch({error, {partial_overrides_unmatched, sample_app, "example",
                          ["certfle"]}}, Result).

overrides_targeting_a_translation_only_is_rejected_test() ->
    %% `versions` matches the partial_translation but NOT a mapping;
    %% overrides apply to mappings only, so this is rejected.
    add_fixture_path(),
    Bad = ?BASE_OPTS ++ [{overrides, [{"versions", {default, []}}]}],
    Result = cuttlefish_partial:load(sample_app, "example", Bad),
    ?assertMatch({error, {partial_overrides_unmatched, _, _, ["versions"]}},
                 Result).

xlate_partial_exclude_unmatched_test() ->
    Msg = ?XLATE({partial_exclude_unmatched, sample_app, "example",
                  ["typo1", "typo2"]}),
    ?assert(string:find(Msg, "sample_app") =/= nomatch),
    ?assert(string:find(Msg, "'typo1'") =/= nomatch),
    ?assert(string:find(Msg, "'typo2'") =/= nomatch).

xlate_partial_overrides_unmatched_test() ->
    Msg = ?XLATE({partial_overrides_unmatched, sample_app, "example",
                  ["certfle"]}),
    ?assert(string:find(Msg, "'certfle'") =/= nomatch),
    ?assert(string:find(Msg, "overrides") =/= nomatch).

load_empty_partial_succeeds_test() ->
    add_fixture_path(),
    ?assertEqual({ok, []},
                 cuttlefish_partial:load(sample_app, "empty", ?BASE_OPTS)).

load_partial_with_syntax_error_returns_error_test() ->
    add_fixture_path(),
    Result = cuttlefish_partial:load(sample_app, "syntax_error", ?BASE_OPTS),
    ?assertMatch({error, {partial_parse_error, sample_app, "syntax_error",
                          {erl_parse, _, _}}}, Result).

exclude_empty_entry_is_rejected_test() ->
    Bad = ?BASE_OPTS ++ [{exclude, [""]}],
    ?assertMatch({error, {partial_unknown_include_opt, {exclude, [""]}}},
                 cuttlefish_partial:load(sample_app, "example", Bad)).

exclude_non_string_entry_is_rejected_test() ->
    Bad = ?BASE_OPTS ++ [{exclude, [some_atom]}],
    ?assertMatch({error, {partial_unknown_include_opt, {exclude, [some_atom]}}},
                 cuttlefish_partial:load(sample_app, "example", Bad)).

exclude_can_match_a_dotted_bare_key_exactly_test() ->
    %% Unusual but supported: a partial with a dotted bare key can be
    %% excluded by its full bare name.
    Terms = [{mapping, "a.b", "a.b", []},
             {mapping, "c", "c", []}],
    Opts = ?BASE_OPTS ++ [{exclude, ["a.b"]}],
    {ok, Out} = cuttlefish_partial:rewrite(Terms, Opts),
    Bares = [strip_prefix(K) || {mapping, K, _, _} <- Out],
    ?assertEqual(["c"], Bares).

%% Helper for the test above.
strip_prefix("x.ssl_options." ++ Rest) -> Rest;
strip_prefix(Other) -> Other.

load_happy_path_returns_rewritten_terms_test() ->
    add_fixture_path(),
    {ok, Terms} = cuttlefish_partial:load(sample_app, "example", ?BASE_OPTS),
    %% The fixture has 4 mappings + 1 partial_translation = 5 terms.
    ?assertEqual(5, length(Terms)),
    Mappings = [T || {mapping, _, _, _} = T <- Terms],
    Translations = [T || {translation, _, _} = T <- Terms],
    ?assertEqual(4, length(Mappings)),
    ?assertEqual(1, length(Translations)),
    %% Every emitted conf and app key starts with the include prefix.
    lists:foreach(
      fun({mapping, ConfKey, AppKey, _}) ->
              ?assert(lists:prefix("x.ssl_options.", ConfKey)),
              ?assert(lists:prefix("y_app.ssl_options.", AppKey))
      end, Mappings).

%% --- bind_translation/3 -------------------------------------------

bind_translation_produces_arity1_test() ->
    Bound = cuttlefish_partial:bind_translation(
              fun(_, _, _) -> ok end, "p", "ap"),
    ?assertEqual({arity, 1}, erlang:fun_info(Bound, arity)).

bind_translation_captures_only_the_prefixes_test() ->
    %% A trivial smoke check that the closure forwards what the caller
    %% provides and nothing more.
    Bound = cuttlefish_partial:bind_translation(
              fun(Conf, P, A) -> {Conf, P, A} end, "p", "ap"),
    ?assertEqual({some_conf, "p", "ap"}, Bound(some_conf)).

%% --- is_partial_filename/1 ----------------------------------------

is_partial_filename_true_for_partial_test() ->
    ?assert(cuttlefish_partial:is_partial_filename("foo.partial")),
    ?assert(cuttlefish_partial:is_partial_filename("/path/to/foo.partial")).

is_partial_filename_false_for_other_extensions_test() ->
    ?assertNot(cuttlefish_partial:is_partial_filename("foo.schema")),
    ?assertNot(cuttlefish_partial:is_partial_filename("foo")),
    ?assertNot(cuttlefish_partial:is_partial_filename("foo.partial.bak")).

%% --- xlate ---------------------------------------------------------

xlate_partial_app_not_loadable_test() ->
    ?assertEqual("Could not load OTP application 'no_such' to resolve "
                 "partial: bad_reason",
                 ?XLATE({partial_app_not_loadable, no_such, bad_reason})).

xlate_partial_app_not_loadable_missing_app_file_hints_at_code_path_test() ->
    %% application:load/1 returns this exact tuple when the .app file
    %% is not on the code path. The hint nudges the user toward the
    %% actual cause rather than dumping the raw `{"no such file...",_}`.
    Msg = ?XLATE({partial_app_not_loadable, rabbit,
                  {"no such file or directory", "rabbit.app"}}),
    ?assert(string:find(Msg, "rabbit") =/= nomatch),
    ?assert(string:find(Msg, "code path") =/= nomatch),
    ?assert(string:find(Msg, "code:add_pathz") =/= nomatch).

xlate_partial_app_no_priv_dir_test() ->
    ?assertEqual("OTP application 'noprivapp' has no priv dir; "
                 "cannot resolve partial",
                 ?XLATE({partial_app_no_priv_dir, noprivapp})).

xlate_partial_file_not_found_test() ->
    ?assertEqual("Partial 'example' not found for app 'sample_app' at: "
                 "/tmp/x.partial",
                 ?XLATE({partial_file_not_found, sample_app, "example",
                         "/tmp/x.partial"})).

xlate_partial_unsupported_translation_points_at_partial_translation_test() ->
    Msg = ?XLATE({partial_unsupported_term, translation}),
    ?assert(string:find(Msg, "partial_translation") =/= nomatch),
    ?assert(string:find(Msg, "{translation,") =/= nomatch).

xlate_partial_unsupported_other_term_test() ->
    Msg = ?XLATE({partial_unsupported_term, config}),
    ?assert(string:find(Msg, "config") =/= nomatch).

xlate_partial_translation_bad_arity_test() ->
    ?assertEqual("Partial translation 'versions' has arity 2; must be 3 "
                 "(Conf, ConfPrefix, AppPrefix)",
                 ?XLATE({partial_translation_bad_arity, "versions", 2})).

xlate_partial_missing_prefix_test() ->
    ?assertEqual("Include of partial 'rabbit:ssl_options' is missing "
                 "the required {prefix, _} argument",
                 ?XLATE({partial_missing_prefix, rabbit, "ssl_options"})).

xlate_partial_missing_app_prefix_test() ->
    ?assertEqual("Include of partial 'rabbit:ssl_options' is missing "
                 "the required {app_prefix, _} argument",
                 ?XLATE({partial_missing_app_prefix, rabbit, "ssl_options"})).

xlate_partial_empty_prefix_test() ->
    ?assertEqual("Include of partial 'rabbit:ssl_options' has an empty "
                 "prefix or app_prefix",
                 ?XLATE({partial_empty_prefix, rabbit, "ssl_options"})).

xlate_partial_unknown_include_opt_test() ->
    Msg = ?XLATE({partial_unknown_include_opt, {predix, "x"}}),
    ?assert(string:find(Msg, "Unknown include_partial option") =/= nomatch),
    ?assert(string:find(Msg, "predix") =/= nomatch).

xlate_partial_file_read_error_includes_reason_test() ->
    Msg = ?XLATE({partial_file_read_error, rabbit, "ssl_options",
                  "/etc/ssl_options.partial", eacces}),
    ?assert(string:find(Msg, "rabbit") =/= nomatch),
    ?assert(string:find(Msg, "ssl_options") =/= nomatch),
    ?assert(string:find(Msg, "eacces") =/= nomatch).

xlate_partial_include_in_partial_test() ->
    ?assertEqual("Partial 'rabbit:ssl_options' contains an "
                 "include_partial term; partials cannot nest",
                 ?XLATE({partial_include_in_partial, rabbit, "ssl_options"})).

xlate_partial_file_too_large_test() ->
    Msg = ?XLATE({partial_file_too_large, {"/tmp/x.partial", 9999999, 8388608}}),
    ?assert(string:find(Msg, "size 9999999") =/= nomatch),
    ?assert(string:find(Msg, "8388608") =/= nomatch).

xlate_partial_file_invalid_unicode_test() ->
    Msg = ?XLATE({partial_file_invalid_unicode, "/tmp/x.partial"}),
    ?assert(string:find(Msg, "not valid UTF-8") =/= nomatch).

xlate_partial_bad_directive_includes_offending_term_test() ->
    Msg = ?XLATE({partial_bad_directive, "wrong-shape"}),
    ?assert(string:find(Msg, "Malformed") =/= nomatch),
    ?assert(string:find(Msg, "wrong-shape") =/= nomatch).

xlate_partial_parse_error_names_partial_test() ->
    %% The error message must point at the offending partial, not
    %% just a line number with no file context.
    Msg = ?XLATE({partial_parse_error, rabbit, "ssl_options",
                  {erl_parse, 12, "syntax error before: ')'"}}),
    ?assert(string:find(Msg, "rabbit") =/= nomatch),
    ?assert(string:find(Msg, "ssl_options") =/= nomatch),
    ?assert(string:find(Msg, "line 12") =/= nomatch).

%% --- helpers ------------------------------------------------------

%% Add the test fixtures' sample_app ebin/ to the code path so
%% `application:load/1` and `code:priv_dir/1` can resolve it.
%% Tests run from the project root; `test/fixtures` resolves there.
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
