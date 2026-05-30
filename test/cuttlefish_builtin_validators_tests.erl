-module(cuttlefish_builtin_validators_tests).

-include_lib("eunit/include/eunit.hrl").

%% Built-in validators delegate to the matching datatype. They are
%% injected lazily — only when the schema references them.

byte_builtin_passes_in_range_test() ->
    %% Boundary values: 0 and 255 are both accepted.
    Schema =
        "{mapping, \"max\", \"app.max\","
        "  [{datatype, integer}, {validators, [\"byte\"]}]}.\n",
    {T, M, V} = cuttlefish_schema:strings([Schema]),
    ?assertEqual([{app, [{max, 0}]}],
                 cuttlefish_generator:map({T, M, V}, [{["max"], "0"}])),
    ?assertEqual([{app, [{max, 255}]}],
                 cuttlefish_generator:map({T, M, V}, [{["max"], "255"}])).

byte_builtin_rejects_out_of_range_test() ->
    Schema =
        "{mapping, \"max\", \"app.max\","
        "  [{datatype, integer}, {validators, [\"byte\"]}]}.\n",
    {T, M, V} = cuttlefish_schema:strings([Schema]),
    Conf = [{["max"], "300"}],
    ?assertMatch({error, validation, _},
                 cuttlefish_generator:map({T, M, V}, Conf)).

port_builtin_passes_test() ->
    %% Boundary values: 0 (OS-assigned semantics) and 65535 are both
    %% accepted by the IANA-range port datatype the builtin delegates
    %% to. A schema that wants stricter behaviour declares its own
    %% predicate, which then wins via shadowing.
    Schema =
        "{mapping, \"p\", \"app.p\","
        "  [{datatype, integer}, {validators, [\"port\"]}]}.\n",
    {T, M, V} = cuttlefish_schema:strings([Schema]),
    ?assertEqual([{app, [{p, 0}]}],
                 cuttlefish_generator:map({T, M, V}, [{["p"], "0"}])),
    ?assertEqual([{app, [{p, 5672}]}],
                 cuttlefish_generator:map({T, M, V}, [{["p"], "5672"}])),
    ?assertEqual([{app, [{p, 65535}]}],
                 cuttlefish_generator:map({T, M, V}, [{["p"], "65535"}])).

port_builtin_rejects_overshoot_test() ->
    Schema =
        "{mapping, \"p\", \"app.p\","
        "  [{datatype, integer}, {validators, [\"port\"]}]}.\n",
    {T, M, V} = cuttlefish_schema:strings([Schema]),
    ?assertMatch({error, validation, _},
                 cuttlefish_generator:map({T, M, V}, [{["p"], "999999"}])).

valid_regex_builtin_accepts_compilable_test() ->
    Schema =
        "{mapping, \"pat\", \"app.pat\","
        "  [{datatype, string}, {validators, [\"valid_regex\"]}]}.\n",
    {T, M, V} = cuttlefish_schema:strings([Schema]),
    ?assertEqual([{app, [{pat, "^[a-z]+$"}]}],
                 cuttlefish_generator:map({T, M, V},
                                          [{["pat"], "^[a-z]+$"}])).

valid_regex_builtin_rejects_uncompilable_test() ->
    Schema =
        "{mapping, \"pat\", \"app.pat\","
        "  [{datatype, string}, {validators, [\"valid_regex\"]}]}.\n",
    {T, M, V} = cuttlefish_schema:strings([Schema]),
    %% Unclosed paren is uncompilable.
    ?assertMatch({error, validation, _},
                 cuttlefish_generator:map({T, M, V},
                                          [{["pat"], "(unclosed"}])).

user_defined_with_same_name_wins_test() ->
    %% A user-defined `"port"' validator that only accepts 1..1024
    %% should win over the builtin.
    Schema =
        "{validator, \"port\", \"strict port\","
        "  fun(N) -> is_integer(N) andalso N >= 1 andalso N =< 1024 end}.\n"
        "{mapping, \"p\", \"app.p\","
        "  [{datatype, integer}, {validators, [\"port\"]}]}.\n",
    {T, M, V} = cuttlefish_schema:strings([Schema]),
    %% 5672 is allowed by the builtin (0..65535) but not the user
    %% predicate (1..1024). Validation must fail using the user
    %% definition.
    ?assertMatch({error, validation, _},
                 cuttlefish_generator:map({T, M, V}, [{["p"], "5672"}])).

builtins_not_injected_when_not_referenced_test() ->
    %% A schema that doesn't mention these names should keep its
    %% validator list unchanged (count = 0 here).
    Schema =
        "{mapping, \"x\", \"app.x\","
        "  [{datatype, integer}]}.\n",
    {_, _, Validators} = cuttlefish_schema:strings([Schema]),
    ?assertEqual(0, length(Validators)).

deprecation_hint_on_injected_builtin_fires_test() ->
    %% When a schema references a builtin and no local definition
    %% shadows it, the builtin's deprecation hint emits one warn
    %% line on the first call site within a pipeline run.
    _ = cuttlefish_test_logging:set_up(),
    _ = cuttlefish_test_logging:bounce(warning),
    Schema =
        "{mapping, \"p\", \"app.p\","
        "  [{datatype, integer}, {validators, [\"port\"]}]}.\n",
    {T, M, V} = cuttlefish_schema:strings([Schema]),
    _ = cuttlefish_generator:map({T, M, V}, [{["p"], "5672"}]),
    Logs = [lists:flatten(L) || L <- cuttlefish_test_logging:get_logs()],
    Lines = [L || L <- Logs, string:find(L, "port") =/= nomatch,
                  string:find(L, "deprecated") =/= nomatch],
    ?assertEqual(1, length(Lines)).

shadow_keeps_builtin_deprecation_silent_test() ->
    %% Conversely, when the local validator wins, the builtin's
    %% deprecation hint must NOT fire — the runtime never reaches
    %% the builtin.
    _ = cuttlefish_test_logging:set_up(),
    _ = cuttlefish_test_logging:bounce(warning),
    Schema =
        "{validator, \"port\", \"strict\","
        "  fun(N) -> is_integer(N) andalso N > 0 andalso N < 65535 end}.\n"
        "{mapping, \"p\", \"app.p\","
        "  [{datatype, integer}, {validators, [\"port\"]}]}.\n",
    {T, M, V} = cuttlefish_schema:strings([Schema]),
    _ = cuttlefish_generator:map({T, M, V}, [{["p"], "5672"}]),
    Logs = [lists:flatten(L) || L <- cuttlefish_test_logging:get_logs()],
    Lines = [L || L <- Logs, string:find(L, "deprecated") =/= nomatch],
    ?assertEqual(0, length(Lines)).

shadow_is_silent_test() ->
    %% A user `port' definition wins and silently overrides the
    %% builtin: no shadow warning, the operator's local predicate is
    %% intentional and the deprecation hint on the builtin (which
    %% only fires when the builtin is the one running) is the
    %% intended channel for nudging the migration.
    _ = cuttlefish_test_logging:set_up(),
    _ = cuttlefish_test_logging:bounce(warning),
    Schema =
        "{validator, \"port\", \"strict\","
        "  fun(N) -> is_integer(N) andalso N >= 1 andalso N =< 1024 end}.\n"
        "{mapping, \"p\", \"app.p\","
        "  [{datatype, integer}, {validators, [\"port\"]}]}.\n",
    _ = cuttlefish_schema:strings([Schema]),
    Logs = [lists:flatten(L) || L <- cuttlefish_test_logging:get_logs()],
    Shadow = [L || L <- Logs, string:find(L, "shadow") =/= nomatch],
    ?assertEqual(0, length(Shadow)).

valid_regex_builtin_rejects_empty_test() ->
    Schema =
        "{mapping, \"pat\", \"app.pat\","
        "  [{datatype, string}, {validators, [\"valid_regex\"]}]}.\n",
    {T, M, V} = cuttlefish_schema:strings([Schema]),
    ?assertMatch({error, validation, _},
                 cuttlefish_generator:map({T, M, V},
                                          [{["pat"], ""}])).

byte_builtin_rejects_negative_test() ->
    Schema =
        "{mapping, \"max\", \"app.max\","
        "  [{datatype, integer}, {validators, [\"byte\"]}]}.\n",
    {T, M, V} = cuttlefish_schema:strings([Schema]),
    ?assertMatch({error, validation, _},
                 cuttlefish_generator:map({T, M, V},
                                          [{["max"], "-1"}])).

builtin_resolved_when_referenced_from_constraint_list_test() ->
    %% A builtin can be referenced from inside a constraint list
    %% (e.g. `{integer, [{validator, "byte"}]}'), the injection
    %% sees the reference and the resolution path uses the
    %% builtin's func.
    Schema =
        "{mapping, \"max\", \"app.max\","
        "  [{datatype, {integer, [{validator, \"byte\"}]}}]}.\n",
    {T, M, V} = cuttlefish_schema:strings([Schema]),
    ?assertEqual([{app, [{max, 200}]}],
                 cuttlefish_generator:map({T, M, V}, [{["max"], "200"}])),
    ?assertMatch({error, transform_datatypes, _},
                 cuttlefish_generator:map({T, M, V}, [{["max"], "300"}])).
