-module(cuttlefish_allow_infinity_tests).

-include_lib("eunit/include/eunit.hrl").

-import(cuttlefish_datatypes, [from_string/2, is_supported/1]).

%% A constraint list with `allow_infinity' accepts the atom or the
%% string `"infinity"' as a valid value. Other inputs still go
%% through the regular numeric pipeline.

is_supported_with_allow_infinity_test() ->
    ?assert(is_supported({integer, [allow_infinity]})),
    ?assert(is_supported({integer, [non_negative, allow_infinity]})),
    ?assert(is_supported({integer, [{min, 1}, {max, 65535}, allow_infinity]})),
    ?assert(is_supported({float, [allow_infinity]})),
    ?assert(is_supported({float, [{min, 0.0}, allow_infinity]})).

integer_accepts_infinity_string_test() ->
    ?assertEqual(infinity,
                 from_string("infinity",
                             {integer, [non_negative, allow_infinity]})).

integer_accepts_infinity_atom_test() ->
    %% Defaults may flow through as atoms; the parser should accept
    %% that shape as well as the string form.
    ?assertEqual(infinity,
                 from_string(infinity,
                             {integer, [{min, 1}, allow_infinity]})).

integer_accepts_numeric_values_alongside_infinity_test() ->
    DT = {integer, [non_negative, allow_infinity]},
    ?assertEqual(0, from_string("0", DT)),
    ?assertEqual(7, from_string("7", DT)).

integer_rejects_negative_when_only_non_negative_test() ->
    %% Negative input still goes through the constraint pipeline.
    DT = {integer, [non_negative, allow_infinity]},
    ?assertMatch({error, {range_violation, _}}, from_string("-1", DT)).

integer_rejects_unknown_strings_test() ->
    DT = {integer, [allow_infinity]},
    ?assertMatch({error, {conversion, _}}, from_string("forever", DT)).

float_accepts_infinity_test() ->
    DT = {float, [{min, 0.0}, allow_infinity]},
    ?assertEqual(infinity, from_string("infinity", DT)),
    ?assertEqual(0.5, from_string("0.5", DT)).

bytesize_accepts_infinity_test() ->
    DT = {bytesize, [non_negative, allow_infinity]},
    ?assertEqual(infinity, from_string("infinity", DT)),
    ?assertEqual(1024, from_string("1KB", DT)).

duration_accepts_infinity_test() ->
    DT = {duration, ms, [non_negative, allow_infinity]},
    ?assertEqual(infinity, from_string("infinity", DT)),
    ?assertEqual(5000, from_string("5s", DT)).

without_allow_infinity_string_is_rejected_test() ->
    %% Sanity: without the shortcut, "infinity" is a parse error.
    ?assertMatch({error, {conversion, _}},
                 from_string("infinity", {integer, [non_negative]})).

infinity_does_not_bypass_other_validators_when_value_is_numeric_test() ->
    %% A numeric value still satisfies the surrounding constraints.
    DT = {integer, [{min, 1}, {max, 100}, allow_infinity]},
    ?assertMatch({error, {range_violation, {0, {min, 1}}}}, from_string("0", DT)).

infinity_to_string_round_trips_test() ->
    %% Regression: `to_string(infinity, {integer, [...]})' must
    %% produce `infinity', not fall through into the integer
    %% clauses where the atom would not be accepted.
    ?assertEqual("infinity",
                 cuttlefish_datatypes:to_string(
                   infinity, {integer, [non_negative, allow_infinity]})),
    ?assertEqual("infinity",
                 cuttlefish_datatypes:to_string(
                   infinity, {float, [{min, 0.0}, allow_infinity]})),
    ?assertEqual("infinity",
                 cuttlefish_datatypes:to_string(
                   infinity, {bytesize, [non_negative, allow_infinity]})),
    ?assertEqual("infinity",
                 cuttlefish_datatypes:to_string(
                   infinity, {duration, ms, [non_negative, allow_infinity]})).
