-module(cuttlefish_validator_aliases_tests).

-include_lib("eunit/include/eunit.hrl").

-define(XLATE(X), lists:flatten(cuttlefish_error:xlate(X))).

%% Per-validator aliases: rename a validator without breaking
%% existing call sites.

parse_with_aliases_test() ->
    V = cuttlefish_validator:parse(
          {validator, "new", "desc", fun(_) -> true end,
           [{aliases, ["old", "older"]}]}),
    ?assertEqual("new", cuttlefish_validator:name(V)),
    ?assertEqual(["old", "older"], cuttlefish_validator:aliases(V)).

parse_without_aliases_defaults_to_empty_test() ->
    V = cuttlefish_validator:parse(
          {validator, "name", "desc", fun(_) -> true end}),
    ?assertEqual([], cuttlefish_validator:aliases(V)).

parse_empty_aliases_is_accepted_test() ->
    V = cuttlefish_validator:parse(
          {validator, "name", "desc", fun(_) -> true end,
           [{aliases, []}]}),
    ?assertEqual([], cuttlefish_validator:aliases(V)).

parse_non_string_alias_is_rejected_test() ->
    Result = cuttlefish_validator:parse(
               {validator, "name", "desc", fun(_) -> true end,
                [{aliases, [42]}]}),
    ?assertMatch({error, {validator_alias_not_a_string, "name", 42}}, Result).

parse_duplicate_aliases_are_rejected_test() ->
    Result = cuttlefish_validator:parse(
               {validator, "name", "desc", fun(_) -> true end,
                [{aliases, ["a", "a"]}]}),
    ?assertMatch({error, {validator_aliases_contain_duplicates, "name"}},
                 Result).

matches_name_via_canonical_test() ->
    V = cuttlefish_validator:parse(
          {validator, "new", "d", fun(_) -> true end, [{aliases, ["old"]}]}),
    ?assert(cuttlefish_validator:matches_name("new", V)),
    ?assert(cuttlefish_validator:matches_name("old", V)),
    ?assertNot(cuttlefish_validator:matches_name("other", V)).

mapping_finds_validator_by_alias_test() ->
    Schema =
        "{validator, \"new\", \"desc\","
        "  fun(N) -> N > 0 end,"
        "  [{aliases, [\"old\"]}]}.\n"
        "{mapping, \"x\", \"app.x\","
        "  [{datatype, integer}, {validators, [\"old\"]}]}.\n",
    {_, [M], [V]} = cuttlefish_schema:strings([Schema]),
    [Found] = cuttlefish_mapping:validators(M, [V]),
    ?assertEqual("new", cuttlefish_validator:name(Found)).

canonical_wins_over_alias_when_both_present_test() ->
    %% Both validators load successfully (their canonical names
    %% don't collide and the alias "old" doesn't collide either).
    %% The lookup must return the canonical "old" definition, not
    %% the one that lists "old" as an alias.
    Schema =
        "{validator, \"old\", \"canonical\","
        "  fun(_) -> a end}.\n"
        "{validator, \"replacement\", \"newer\","
        "  fun(_) -> b end,"
        "  [{aliases, [\"old_other\"]}]}.\n"
        "{mapping, \"x\", \"app.x\","
        "  [{datatype, integer}, {validators, [\"old\"]}]}.\n",
    {_, [M], Validators} = cuttlefish_schema:strings([Schema]),
    [Found] = cuttlefish_mapping:validators(M, Validators),
    ?assertEqual("old", cuttlefish_validator:name(Found)).

alias_collision_between_validators_is_rejected_test() ->
    Schema =
        "{validator, \"a\", \"d\", fun(_) -> true end,"
        "  [{aliases, [\"shared\"]}]}.\n"
        "{validator, \"b\", \"d\", fun(_) -> true end,"
        "  [{aliases, [\"shared\"]}]}.\n",
    Result = cuttlefish_schema:strings([Schema]),
    ?assertMatch({errorlist,
                  [{error, {validator_alias_collision, "shared", _, _}}]},
                 Result).

alias_shadowing_canonical_name_is_rejected_test() ->
    Schema =
        "{validator, \"x\", \"d\", fun(_) -> true end}.\n"
        "{validator, \"y\", \"d\", fun(_) -> true end,"
        "  [{aliases, [\"x\"]}]}.\n",
    Result = cuttlefish_schema:strings([Schema]),
    ?assertMatch({errorlist,
                  [{error, {validator_alias_shadows_name, "x", "y"}}]},
                 Result).

xlate_validator_alias_collision_test() ->
    Msg = ?XLATE({validator_alias_collision, "shared", "a", "b"}),
    ?assert(string:find(Msg, "shared") =/= nomatch),
    ?assert(string:find(Msg, "a") =/= nomatch),
    ?assert(string:find(Msg, "b") =/= nomatch).

user_alias_matching_builtin_name_suppresses_injection_test() ->
    %% A user validator with alias "port" should answer to "port"
    %% references AND prevent the builtin from being injected. This
    %% is consistent with `matches_name/2' precedence.
    Schema =
        "{validator, \"strict_port\", \"my strict\","
        "  fun(N) -> N >= 1024 andalso N =< 49151 end,"
        "  [{aliases, [\"port\"]}]}.\n"
        "{mapping, \"p\", \"app.p\","
        "  [{datatype, integer}, {validators, [\"port\"]}]}.\n",
    {_, [M], V} = cuttlefish_schema:strings([Schema]),
    [Found] = cuttlefish_mapping:validators(M, V),
    ?assertEqual("strict_port", cuttlefish_validator:name(Found)),
    %% The builtin must NOT have been added — the user's "strict_port"
    %% is the only validator in the list.
    ?assertEqual(1, length(V)).
