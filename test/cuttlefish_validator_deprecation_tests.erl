-module(cuttlefish_validator_deprecation_tests).

-include_lib("eunit/include/eunit.hrl").

%% Deprecation hints on validator definitions surface as a single
%% warn line per validator name per load cycle.

parse_with_deprecation_hint_test() ->
    V = cuttlefish_validator:parse(
          {validator, "old", "d", fun(_) -> true end,
           [{deprecated, "3.9.0", "use the new form"}]}),
    ?assertEqual({"3.9.0", "use the new form"},
                 cuttlefish_validator:deprecated(V)).

parse_without_hint_defaults_to_undefined_test() ->
    V = cuttlefish_validator:parse(
          {validator, "x", "d", fun(_) -> true end}),
    ?assertEqual(undefined, cuttlefish_validator:deprecated(V)).

%% A malformed deprecation hint inside a schema file must surface
%% as a clean errorlist — never as an uncaught exception in the
%% downstream filter pipeline. (Regression guard against a bug
%% where the error tuple polluted the validators list and broke
%% record-field access in validate_validator_aliases/1.)
malformed_options_in_schema_surfaces_as_errorlist_test() ->
    Schema =
        "{validator, \"x\", \"d\", fun(_) -> true end,"
        "  [{deprecated, not_a_string, \"hint\"}]}.\n"
        "{mapping, \"k\", \"app.k\","
        "  [{datatype, integer}, {validators, [\"x\"]}]}.\n",
    Result = try cuttlefish_schema:strings([Schema])
             catch C:E -> {caught, C, E}
             end,
    ?assertMatch({errorlist, [_|_]}, Result),
    {errorlist, Es} = Result,
    ?assert(lists:any(
        fun({error, {validator_deprecated_malformed, "x", _}}) -> true;
           (_) -> false
        end, Es)).

parse_malformed_deprecation_hint_is_rejected_test() ->
    Bad = cuttlefish_validator:parse(
            {validator, "x", "d", fun(_) -> true end,
             [{deprecated, not_a_string, "hint"}]}),
    ?assertMatch({error, {validator_deprecated_malformed, "x", _}}, Bad).

unknown_option_is_rejected_test() ->
    Result = cuttlefish_validator:parse(
               {validator, "x", "d", fun(_) -> true end,
                [{not_an_option, ok}]}),
    ?assertMatch({error, {validator_options_invalid, "x", _}}, Result).

warning_emitted_on_first_use_test() ->
    _ = cuttlefish_test_logging:set_up(),
    _ = cuttlefish_test_logging:bounce(warning),
    Schema =
        "{validator, \"old_check\", \"a check\","
        "  fun(N) -> N > 0 end,"
        "  [{deprecated, \"3.9.0\", \"use {integer, positive}\"}]}.\n"
        "{mapping, \"x\", \"app.x\","
        "  [{datatype, integer}, {validators, [\"old_check\"]}]}.\n",
    {T, M, V} = cuttlefish_schema:strings([Schema]),
    Conf = [{["x"], "5"}],
    _ = cuttlefish_generator:map({T, M, V}, Conf),
    Logs = [lists:flatten(L) || L <- cuttlefish_test_logging:get_logs()],
    Mentions = [L || L <- Logs,
                     string:find(L, "old_check") =/= nomatch,
                     string:find(L, "deprecated") =/= nomatch],
    ?assertEqual(1, length(Mentions)).

deprecation_warning_includes_hint_text_test() ->
    _ = cuttlefish_test_logging:set_up(),
    _ = cuttlefish_test_logging:bounce(warning),
    Schema =
        "{validator, \"old_check\", \"a check\","
        "  fun(N) -> N > 0 end,"
        "  [{deprecated, \"3.9.0\", \"use {integer, positive} datatype\"}]}.\n"
        "{mapping, \"x\", \"app.x\","
        "  [{datatype, integer}, {validators, [\"old_check\"]}]}.\n",
    {T, M, V} = cuttlefish_schema:strings([Schema]),
    _ = cuttlefish_generator:map({T, M, V}, [{["x"], "5"}]),
    Logs = [lists:flatten(L) || L <- cuttlefish_test_logging:get_logs()],
    Mentions = [L || L <- Logs, string:find(L, "old_check") =/= nomatch],
    [Line | _] = Mentions,
    ?assert(string:find(Line, "use {integer, positive} datatype") =/= nomatch),
    ?assert(string:find(Line, "3.9.0") =/= nomatch).

warning_emitted_at_most_once_per_load_test() ->
    %% Two mappings both pointing at the same deprecated validator,
    %% one map call: exactly one warn is expected.
    _ = cuttlefish_test_logging:set_up(),
    _ = cuttlefish_test_logging:bounce(warning),
    Schema =
        "{validator, \"old_check\", \"a check\","
        "  fun(N) -> N > 0 end,"
        "  [{deprecated, \"3.9.0\", \"hint\"}]}.\n"
        "{mapping, \"a\", \"app.a\","
        "  [{datatype, integer}, {validators, [\"old_check\"]}]}.\n"
        "{mapping, \"b\", \"app.b\","
        "  [{datatype, integer}, {validators, [\"old_check\"]}]}.\n",
    {T, M, V} = cuttlefish_schema:strings([Schema]),
    Conf = [{["a"], "5"}, {["b"], "10"}],
    _ = cuttlefish_generator:map({T, M, V}, Conf),
    Logs = [lists:flatten(L) || L <- cuttlefish_test_logging:get_logs()],
    Mentions = [L || L <- Logs, string:find(L, "old_check") =/= nomatch],
    ?assertEqual(1, length(Mentions)).
