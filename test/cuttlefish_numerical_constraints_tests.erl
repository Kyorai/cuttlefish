-module(cuttlefish_numerical_constraints_tests).

-include_lib("eunit/include/eunit.hrl").

-import(cuttlefish_datatypes, [from_string/2, to_string/2, is_supported/1]).

-define(XLATE(X), lists:flatten(cuttlefish_error:xlate(X))).


%% is_supported/1

port_byte_percent_aliases_are_supported_test() ->
    ?assert(is_supported(port)),
    ?assert(is_supported(byte)),
    ?assert(is_supported(percent)).

bounded_integer_is_supported_test() ->
    ?assert(is_supported({integer, [{min, 0}]})),
    ?assert(is_supported({integer, [{max, 100}]})),
    ?assert(is_supported({integer, [{min, 0}, {max, 65535}]})),
    ?assert(is_supported({integer, [{gt, 0}]})),
    ?assert(is_supported({integer, [{lt, 1024}]})),
    ?assert(is_supported({integer, [{min, 0}, {max, 65535}, {gt, -1}]})).

bounded_float_is_supported_test() ->
    ?assert(is_supported({float, [{min, 0.0}]})),
    ?assert(is_supported({float, [{max, 1.0}]})),
    ?assert(is_supported({float, [{min, 0.0}, {max, 1.0}]})),
    %% Integer bounds on a float type are allowed; comparison is polymorphic
    ?assert(is_supported({float, [{min, 0}]})).

integer_bound_works_on_float_type_test() ->
    T = {float, [{min, 0}, {max, 1}]},
    ?assertEqual(0.5, from_string("0.5", T)),
    ?assertMatch({error, {range_violation, _}}, from_string(1.5, T)).

empty_constraint_list_is_supported_test() ->
    ?assert(is_supported({integer, []})),
    ?assert(is_supported({float, []})).

shortcut_constraints_are_supported_test() ->
    ?assert(is_supported({integer, non_negative})),
    ?assert(is_supported({integer, positive})),
    ?assert(is_supported({float, non_negative})),
    ?assert(is_supported({float, positive})),
    ?assert(is_supported({integer, [non_negative, {max, 1000}]})),
    ?assert(is_supported({integer, [positive]})).

invalid_constraints_are_rejected_test() ->
    ?assertNot(is_supported({integer, [{min, "zero"}]})),
    ?assertNot(is_supported({integer, [{nonsense, 42}]})),
    ?assertNot(is_supported({integer, garbage_shortcut})),
    ?assertNot(is_supported({integer, [{min, 1.5}]})),
    ?assertNot(is_supported({float, [{min, "zero"}]})),
    ?assertNot(is_supported({float, [bogus]})).


%% port

port_accepts_valid_string_input_test() ->
    ?assertEqual(0,     from_string("0",     port)),
    ?assertEqual(5672,  from_string("5672",  port)),
    ?assertEqual(65535, from_string("65535", port)).

port_accepts_valid_integer_input_test() ->
    ?assertEqual(5672, from_string(5672, port)).

port_rejects_negative_values_test() ->
    Result = from_string(-1, port),
    ?assertMatch({error, {range_violation, {-1, {min, 0}}}}, Result),
    ?assertEqual("-1 is below the minimum allowed value of 0", ?XLATE(Result)).

port_rejects_values_above_65535_test() ->
    Result = from_string(65536, port),
    ?assertMatch({error, {range_violation, {65536, {max, 65535}}}}, Result),
    ?assertEqual("65536 exceeds the maximum allowed value of 65535", ?XLATE(Result)).

port_rejects_non_numeric_input_test() ->
    ?assertMatch({error, {conversion, {"http", integer}}},
                 from_string("http", port)).

port_to_string_round_trip_test() ->
    ?assertEqual("5672", to_string(5672, port)),
    ?assertEqual("0", to_string(0, port)).


%% byte

byte_accepts_full_range_test() ->
    ?assertEqual(0,   from_string("0",   byte)),
    ?assertEqual(127, from_string("127", byte)),
    ?assertEqual(255, from_string("255", byte)).

byte_rejects_negative_values_test() ->
    ?assertMatch({error, {range_violation, {-1, {min, 0}}}},
                 from_string(-1, byte)).

byte_rejects_values_above_255_test() ->
    Result = from_string(256, byte),
    ?assertMatch({error, {range_violation, {256, {max, 255}}}}, Result),
    ?assertEqual("256 exceeds the maximum allowed value of 255", ?XLATE(Result)).

byte_to_string_test() ->
    ?assertEqual("42", to_string(42, byte)).


%% percent (alias for {percent, integer})

percent_alias_accepts_zero_to_hundred_test() ->
    ?assertEqual(0,   from_string("0%",   percent)),
    ?assertEqual(50,  from_string("50%",  percent)),
    ?assertEqual(100, from_string("100%", percent)).

percent_alias_rejects_out_of_range_test() ->
    ?assertMatch({error, {range, _}}, from_string("110%", percent)),
    ?assertMatch({error, {range, _}}, from_string("-1%",  percent)).

percent_alias_to_string_test() ->
    ?assertEqual("10%", to_string(10, percent)).


%% Generic {integer, [...]}

bounded_integer_accepts_in_range_test() ->
    T = {integer, [{min, 10}, {max, 20}]},
    ?assertEqual(10, from_string("10", T)),
    ?assertEqual(15, from_string(15,   T)),
    ?assertEqual(20, from_string("20", T)).

bounded_integer_rejects_below_min_test() ->
    T = {integer, [{min, 10}, {max, 20}]},
    ?assertMatch({error, {range_violation, {9, {min, 10}}}}, from_string(9, T)).

bounded_integer_rejects_above_max_test() ->
    T = {integer, [{min, 10}, {max, 20}]},
    ?assertMatch({error, {range_violation, {21, {max, 20}}}}, from_string(21, T)).

bounded_integer_reports_first_failing_constraint_test() ->
    %% Constraint order in the declaration determines which violation is
    %% reported when several would fail.
    T1 = {integer, [{min, 10}, {max, 20}]},
    T2 = {integer, [{max, 20}, {min, 10}]},
    ?assertMatch({error, {range_violation, {5, {min, 10}}}}, from_string(5, T1)),
    ?assertMatch({error, {range_violation, {5, {min, 10}}}}, from_string(5, T2)),
    ?assertMatch({error, {range_violation, {99, {max, 20}}}}, from_string(99, T1)),
    ?assertMatch({error, {range_violation, {99, {max, 20}}}}, from_string(99, T2)).

bounded_integer_strict_comparisons_test() ->
    Gt = {integer, [{gt, 0}]},
    Lt = {integer, [{lt, 0}]},
    ?assertEqual(1, from_string(1, Gt)),
    ?assertMatch({error, {range_violation, {0, {gt, 0}}}}, from_string(0, Gt)),
    ?assertEqual(-1, from_string(-1, Lt)),
    ?assertMatch({error, {range_violation, {0, {lt, 0}}}}, from_string(0, Lt)).

bounded_integer_strict_message_test() ->
    ?assertEqual("0 must be strictly greater than 0",
                 ?XLATE(from_string(0, {integer, [{gt, 0}]}))),
    ?assertEqual("0 must be strictly less than 0",
                 ?XLATE(from_string(0, {integer, [{lt, 0}]}))).

empty_integer_constraints_behave_like_plain_integer_test() ->
    ?assertEqual(42, from_string("42", {integer, []})),
    ?assertEqual(-7, from_string(-7,   {integer, []})),
    ?assertMatch({error, {conversion, _}}, from_string("nope", {integer, []})).

integer_shortcut_non_negative_test() ->
    T = {integer, non_negative},
    ?assertEqual(0, from_string(0, T)),
    ?assertEqual(7, from_string("7", T)),
    ?assertMatch({error, {range_violation, {-1, {min, 0}}}}, from_string(-1, T)).

integer_shortcut_positive_test() ->
    T = {integer, positive},
    ?assertEqual(1, from_string(1, T)),
    ?assertMatch({error, {range_violation, {0, {min, 1}}}}, from_string(0, T)),
    ?assertMatch({error, {range_violation, {-5, {min, 1}}}}, from_string(-5, T)).

shortcuts_compose_inside_constraint_list_test() ->
    T = {integer, [non_negative, {max, 100}]},
    ?assertEqual(0,   from_string(0, T)),
    ?assertEqual(100, from_string(100, T)),
    ?assertMatch({error, {range_violation, {-1, {min, 0}}}},  from_string(-1, T)),
    ?assertMatch({error, {range_violation, {101, {max, 100}}}}, from_string(101, T)).

list_wrapped_positive_shortcut_test() ->
    T = {integer, [positive]},
    ?assertEqual(1, from_string(1, T)),
    ?assertMatch({error, {range_violation, {0, {min, 1}}}}, from_string(0, T)).

multiple_shortcuts_in_one_list_test() ->
    %% Redundant but legal; both expand to {min, 0}/{min, 1}.
    T = {integer, [non_negative, positive]},
    ?assertEqual(1, from_string(1, T)),
    ?assertMatch({error, {range_violation, {0, {min, 1}}}}, from_string(0, T)).

bounded_integer_to_string_test() ->
    ?assertEqual("65535", to_string(65535, {integer, [{min, 0}, {max, 65535}]})),
    ?assertEqual("0",     to_string(0,     {integer, non_negative})).


%% Generic {float, [...]}

bounded_float_accepts_in_range_test() ->
    T = {float, [{min, 0.0}, {max, 1.0}]},
    ?assertEqual(0.0, from_string("0.0", T)),
    ?assertEqual(0.5, from_string(0.5,   T)),
    ?assertEqual(1.0, from_string("1.0", T)).

bounded_float_rejects_out_of_range_test() ->
    T = {float, [{min, 0.0}, {max, 1.0}]},
    ?assertMatch({error, {range_violation, {-0.1, {min, +0.0}}}}, from_string(-0.1, T)),
    ?assertMatch({error, {range_violation, {1.5,  {max, +1.0}}}}, from_string(1.5,  T)).

float_shortcut_non_negative_test() ->
    T = {float, non_negative},
    ?assertEqual(0.0, from_string(0.0, T)),
    ?assertMatch({error, {range_violation, {-0.5, {min, +0.0}}}}, from_string(-0.5, T)).

%% `positive' on a float means strictly greater than 0 (unlike on integer
%% where the natural smallest positive value is 1). Documented behaviour.
float_shortcut_positive_is_strict_test() ->
    T = {float, positive},
    ?assertEqual(0.1, from_string(0.1, T)),
    ?assertMatch({error, {range_violation, {+0.0, {gt, +0.0}}}}, from_string(0.0, T)).

bounded_float_to_string_test() ->
    ?assertEqual("0.5", to_string(0.5, {float, [{min, 0.0}, {max, 1.0}]})),
    ?assertEqual("0.0", to_string(0.0, {float, non_negative})).

string_to_bounded_float_test() ->
    T = {float, [{min, 0.0}, {max, 1.0}]},
    ?assertEqual(0.25, from_string("0.25", T)),
    ?assertMatch({error, {range_violation, _}}, from_string("1.5", T)).

empty_float_constraints_behave_like_plain_float_test() ->
    ?assertEqual(3.14, from_string("3.14", {float, []})),
    ?assertEqual(3.14, from_string(3.14,   {float, []})),
    ?assertMatch({error, {conversion, _}}, from_string("nope", {float, []})).


%% Regression: the new {integer, [Constraints]} and {float, [Constraints]}
%% shapes must not be mistaken for the existing extended forms
%% {integer, integer()} and {float, float()}.

is_extended_does_not_match_constraint_form_test() ->
    ?assertNot(cuttlefish_datatypes:is_extended({integer, [{min, 0}]})),
    ?assertNot(cuttlefish_datatypes:is_extended({integer, non_negative})),
    ?assertNot(cuttlefish_datatypes:is_extended({float,   [{min, 0.0}]})),
    ?assertNot(cuttlefish_datatypes:is_extended({float,   non_negative})).

is_extended_still_matches_classic_extended_forms_test() ->
    ?assert(cuttlefish_datatypes:is_extended({integer, 42})),
    ?assert(cuttlefish_datatypes:is_extended({float,   3.14})).


%% Conversion-error precedence: a value that cannot be parsed produces a
%% conversion error, not a range error.

conversion_error_short_circuits_constraint_check_test() ->
    Result = from_string("not_a_number", {integer, [{min, 0}, {max, 10}]}),
    ?assertMatch({error, {conversion, {"not_a_number", integer}}}, Result),
    Result2 = from_string("nope", {float, [{min, +0.0}]}),
    ?assertMatch({error, {conversion, {"nope", float}}}, Result2).


%% Composition with datatype lists.
%%
%% The classic `non_negative_integer' validator in `rabbit.schema' accepts
%% the atom `infinity' as well as non-negative integers. The migration
%% path for those mappings is the existing datatype-list mechanism:
%% `{datatype, [{atom, infinity}, {integer, non_negative}]}'. These tests
%% pin that composition.

constrained_integer_is_valid_inside_datatype_list_test() ->
    ?assert(cuttlefish_datatypes:is_valid_list(
              [{atom, infinity}, {integer, non_negative}])),
    ?assert(cuttlefish_datatypes:is_valid_list(
              [{atom, infinity}, port])),
    ?assert(cuttlefish_datatypes:is_valid_list(
              [{atom, infinity}, {integer, [{min, 0}, {max, 1024}]}])).

infinity_or_non_negative_integer_end_to_end_test() ->
    %% Migration shape for the `non_negative_integer' validator that also
    %% accepts `infinity'. Verified through the full pipeline so that the
    %% list-of-datatypes resolution order (first match wins) is exercised.
    Schema = cuttlefish_schema:strings([
        "{mapping, \"x.limit\", \"app.limit\","
        "  [{datatype, [{atom, infinity}, {integer, non_negative}]}]}."
    ]),
    ?assertEqual(infinity,
                 lookup_app_value(app, limit,
                                  cuttlefish_generator:map(
                                    Schema, [{["x", "limit"], "infinity"}]))),
    ?assertEqual(42,
                 lookup_app_value(app, limit,
                                  cuttlefish_generator:map(
                                    Schema, [{["x", "limit"], "42"}]))).

port_end_to_end_test() ->
    Schema = cuttlefish_schema:strings([
        "{mapping, \"x.port\", \"app.port\", [{datatype, port}]}."
    ]),
    ?assertEqual(5672,
                 lookup_app_value(app, port,
                                  cuttlefish_generator:map(
                                    Schema, [{["x", "port"], "5672"}]))),
    ?assertMatch({error, transform_datatypes, _},
                 cuttlefish_generator:map(
                   Schema, [{["x", "port"], "65536"}])).

list_of_ports_test() ->
    ?assertEqual([5672, 5673],
                 from_string("5672, 5673", {list, port})),
    %% A list mixes good entries and errors verbatim, matching plain
    %% {list, integer} semantics. The pipeline surfaces the error.
    ?assertMatch([5672, {error, {range_violation, _}}],
                 from_string("5672, 99999", {list, port})).

list_of_constrained_integers_test() ->
    T = {list, {integer, [{min, 0}, {max, 100}]}},
    ?assertEqual([10, 20, 30], from_string("10, 20, 30", T)),
    ?assertMatch([_, {error, {range_violation, _}}, _],
                 from_string("10, 200, 30", T)).


pretty_datatype_port_test() ->
    ?assertEqual("an integer in 0-65535 (TCP/UDP port)",
                 cuttlefish_conf:pretty_datatype(port)).

pretty_datatype_byte_test() ->
    ?assertEqual("an integer in 0-255",
                 cuttlefish_conf:pretty_datatype(byte)).

pretty_datatype_percent_test() ->
    ?assertEqual("an integer percent in 0-100, with a trailing '%' on string input",
                 cuttlefish_conf:pretty_datatype(percent)).

pretty_datatype_constrained_integer_test() ->
    ?assertEqual("an integer (min=0, max=65535)",
                 cuttlefish_conf:pretty_datatype(
                   {integer, [{min, 0}, {max, 65535}]})),
    ?assertEqual("an integer (non-negative)",
                 cuttlefish_conf:pretty_datatype({integer, non_negative})),
    ?assertEqual("an integer (positive)",
                 cuttlefish_conf:pretty_datatype({integer, positive})),
    ?assertEqual("an integer",
                 cuttlefish_conf:pretty_datatype({integer, []})).

pretty_datatype_constrained_float_test() ->
    ?assertEqual("a float (min=0.0, max=1.0)",
                 cuttlefish_conf:pretty_datatype(
                   {float, [{min, 0.0}, {max, 1.0}]})),
    ?assertEqual("a float (>0, <100)",
                 cuttlefish_conf:pretty_datatype(
                   {float, [{gt, 0}, {lt, 100}]})).

pretty_datatype_classic_extended_integer_still_works_test() ->
    ?assertEqual("the integer 42",
                 cuttlefish_conf:pretty_datatype({integer, 42})).


lookup_app_value(App, Key, GeneratorResult) ->
    proplists:get_value(Key, proplists:get_value(App, GeneratorResult)).
