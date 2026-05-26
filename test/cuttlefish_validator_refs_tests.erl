-module(cuttlefish_validator_refs_tests).

-include_lib("eunit/include/eunit.hrl").

-define(XLATE(X), lists:flatten(cuttlefish_error:xlate(X))).

missing_validator_is_rejected_at_schema_load_test() ->
    %% A mapping that references an undefined validator fails at
    %% schema-load time rather than waiting for validation phase.
    Schema = "{mapping, \"x\", \"app.x\","
             " [{datatype, integer}, {validators, [\"no_such_validator\"]}]}.\n",
    Result = cuttlefish_schema:strings([Schema]),
    ?assertMatch({errorlist,
                  [{error, {validator_not_defined, "x", "no_such_validator"}}]},
                 Result).

defined_validator_passes_test() ->
    Schema =
        "{validator, \"positive\", \"must be positive\","
        " fun(N) -> N > 0 end}.\n"
        "{mapping, \"x\", \"app.x\","
        " [{datatype, integer}, {validators, [\"positive\"]}]}.\n",
    ?assertMatch({_, [_], [_]}, cuttlefish_schema:strings([Schema])).

validator_defined_in_sibling_schema_passes_test() ->
    %% Multiple-schema merge: the validator is defined in one schema,
    %% referenced in another. Pre-flight runs after the merge so both
    %% are visible.
    Validator =
        "{validator, \"positive\", \"must be positive\","
        " fun(N) -> N > 0 end}.\n",
    Consumer =
        "{mapping, \"x\", \"app.x\","
        " [{datatype, integer}, {validators, [\"positive\"]}]}.\n",
    ?assertMatch({_, [_], [_]},
                 cuttlefish_schema:strings([Validator, Consumer])).

multiple_missing_validators_are_all_reported_test() ->
    Schema = "{mapping, \"x\", \"app.x\","
             " [{datatype, integer},"
             "  {validators, [\"missing_a\", \"missing_b\"]}]}.\n",
    {errorlist, Errors} = cuttlefish_schema:strings([Schema]),
    ?assertEqual(2, length(Errors)),
    ?assert(lists:any(
        fun({error, {validator_not_defined, "x", "missing_a"}}) -> true;
           (_) -> false
        end, Errors)),
    ?assert(lists:any(
        fun({error, {validator_not_defined, "x", "missing_b"}}) -> true;
           (_) -> false
        end, Errors)).

xlate_validator_not_defined_is_actionable_test() ->
    Msg = ?XLATE({validator_not_defined, "auth_http.ssl_options.cacertfile",
                  "pem_file"}),
    ?assert(string:find(Msg, "auth_http.ssl_options.cacertfile") =/= nomatch),
    ?assert(string:find(Msg, "pem_file") =/= nomatch).
