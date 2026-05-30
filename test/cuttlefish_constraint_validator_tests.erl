-module(cuttlefish_constraint_validator_tests).

-include_lib("eunit/include/eunit.hrl").

-import(cuttlefish_datatypes, [from_string/2, is_supported/1]).

%% Validator entries inside a constraint list. Two forms are
%% accepted: `{validator, Fun}' is applied directly at conversion
%% time; `{validator, Name}' is resolved against the loaded
%% validator set before datatype conversion runs.

is_supported_with_fun_validator_test() ->
    F = fun(N) when is_integer(N) -> N rem 2 =:= 0 end,
    ?assert(is_supported({integer, [{validator, F}]})),
    ?assert(is_supported({integer, [{min, 0}, {validator, F}, {max, 100}]})).

is_supported_with_named_validator_test() ->
    ?assert(is_supported({integer, [{validator, "power_of_two"}]})).

fun_validator_passes_value_through_test() ->
    DT = {integer, [{min, 1}, {validator, fun(N) -> N rem 2 =:= 0 end}]},
    ?assertEqual(8, from_string("8", DT)),
    ?assertEqual(2, from_string("2", DT)).

fun_validator_rejects_failing_value_test() ->
    DT = {integer, [{validator, fun(N) -> N rem 2 =:= 0 end}]},
    ?assertMatch({error, {constraint_validator_failed, 3}}, from_string("3", DT)).

fun_validator_runs_left_to_right_first_failure_wins_test() ->
    %% The range constraint fires before the validator, so a value
    %% that fails the range never reaches the validator.
    DT = {integer, [{min, 1}, {validator, fun(_) -> true end}]},
    ?assertMatch({error, {range_violation, {0, {min, 1}}}}, from_string("0", DT)).

fun_validator_exceptions_count_as_failure_test() ->
    DT = {integer, [{validator, fun(_) -> error(boom) end}]},
    ?assertMatch({error, {constraint_validator_failed, _}}, from_string("1", DT)).

named_validator_unresolved_at_runtime_is_an_error_test() ->
    %% If a name slips through schema validation and reaches
    %% conversion unresolved, the runtime surfaces it rather than
    %% silently accepting the value.
    DT = {integer, [{validator, "not_resolved"}]},
    ?assertMatch({error, {constraint_validator_unresolved, "not_resolved"}},
                 from_string("1", DT)).

%% End-to-end through the schema pipeline.

named_validator_in_constraint_list_resolves_test() ->
    Schema =
        "{validator, \"power_of_two\", \"a power of two\","
        "  fun(N) when is_integer(N) ->"
        "    N > 0 andalso (N band (N - 1)) =:= 0"
        "  end}.\n"
        "{mapping, \"queue.max_size\", \"app.qmax\","
        "  [{datatype, {integer, [{min, 1}, {validator, \"power_of_two\"}]}}]}.\n",
    {Translations, Mappings, Validators} = cuttlefish_schema:strings([Schema]),
    Conf = [{["queue", "max_size"], "16"}],
    Result = cuttlefish_generator:map(
               {Translations, Mappings, Validators}, Conf),
    ?assertEqual([{app, [{qmax, 16}]}], Result).

named_validator_in_constraint_rejects_bad_value_test() ->
    Schema =
        "{validator, \"power_of_two\", \"a power of two\","
        "  fun(N) when is_integer(N) ->"
        "    N > 0 andalso (N band (N - 1)) =:= 0"
        "  end}.\n"
        "{mapping, \"queue.max_size\", \"app.qmax\","
        "  [{datatype, {integer, [{min, 1}, {validator, \"power_of_two\"}]}}]}.\n",
    {T, M, V} = cuttlefish_schema:strings([Schema]),
    Conf = [{["queue", "max_size"], "5"}],
    ?assertMatch({error, transform_datatypes, _},
                 cuttlefish_generator:map({T, M, V}, Conf)).

undefined_named_validator_in_constraint_is_pre_flight_error_test() ->
    Schema =
        "{mapping, \"queue.max_size\", \"app.qmax\","
        "  [{datatype, {integer, [{validator, \"never_defined\"}]}}]}.\n",
    Result = cuttlefish_schema:strings([Schema]),
    ?assertMatch({errorlist,
                  [{error, {validator_not_defined, "queue.max_size",
                            "never_defined"}}]},
                 Result).

%% Nested datatypes — `{list, {integer, [{validator, Name}]}}'.
%% The name lookup must recurse into the inner datatype.
nested_list_resolves_named_validator_test() ->
    Schema =
        "{validator, \"even\", \"even integer\","
        "  fun(N) -> is_integer(N) andalso N rem 2 =:= 0 end}.\n"
        "{mapping, \"vals\", \"app.vals\","
        "  [{datatype, {list, {integer, [{validator, \"even\"}]}}}]}.\n",
    {T, M, V} = cuttlefish_schema:strings([Schema]),
    Conf = [{["vals"], "2, 4, 6"}],
    ?assertEqual([{app, [{vals, [2, 4, 6]}]}],
                 cuttlefish_generator:map({T, M, V}, Conf)).

nested_list_undefined_name_is_pre_flight_error_test() ->
    Schema =
        "{mapping, \"vals\", \"app.vals\","
        "  [{datatype, {list, {integer, [{validator, \"missing\"}]}}}]}.\n",
    ?assertMatch({errorlist,
                  [{error, {validator_not_defined, "vals", "missing"}}]},
                 cuttlefish_schema:strings([Schema])).

multiple_validators_run_in_order_test() ->
    %% Two anonymous validators: first rejects everything > 100,
    %% second rejects odd numbers. A value 99 passes the first but
    %% fails the second.
    DT = {integer, [{validator, fun(N) -> N =< 100 end},
                    {validator, fun(N) -> N rem 2 =:= 0 end}]},
    ?assertEqual(2, cuttlefish_datatypes:from_string("2", DT)),
    ?assertMatch({error, {constraint_validator_failed, 99}},
                 cuttlefish_datatypes:from_string("99", DT)),
    ?assertMatch({error, {constraint_validator_failed, 101}},
                 cuttlefish_datatypes:from_string("101", DT)).

constraint_list_can_reference_validator_by_alias_test() ->
    Schema =
        "{validator, \"even\", \"even\","
        "  fun(N) -> is_integer(N) andalso N rem 2 =:= 0 end,"
        "  [{aliases, [\"evens\"]}]}.\n"
        "{mapping, \"n\", \"app.n\","
        "  [{datatype, {integer, [{validator, \"evens\"}]}}]}.\n",
    {T, M, V} = cuttlefish_schema:strings([Schema]),
    ?assertEqual([{app, [{n, 4}]}],
                 cuttlefish_generator:map({T, M, V}, [{["n"], "4"}])),
    ?assertMatch({error, transform_datatypes, _},
                 cuttlefish_generator:map({T, M, V}, [{["n"], "3"}])).

allow_infinity_skips_validator_test() ->
    %% When the bypass fires, the constraint pipeline (including
    %% validators) does not run on the atom `infinity'. The
    %% intent is for infinity to be a *valid* value regardless of
    %% the other constraints.
    DT = {integer, [allow_infinity,
                    {validator, fun(_) -> false end}]},
    ?assertEqual(infinity, cuttlefish_datatypes:from_string("infinity", DT)).
