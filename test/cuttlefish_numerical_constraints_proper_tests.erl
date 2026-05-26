-module(cuttlefish_numerical_constraints_proper_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PROP(P), ?assert(proper:quickcheck(P, [{numtests, 500}, {to_file, user}]))).

-import(cuttlefish_datatypes, [from_string/2]).


%% Test entry points

port_round_trip_test()         -> ?PROP(prop_port_round_trip()).
port_rejects_out_of_range_test() -> ?PROP(prop_port_rejects_out_of_range()).
byte_round_trip_test()         -> ?PROP(prop_byte_round_trip()).
byte_rejects_out_of_range_test() -> ?PROP(prop_byte_rejects_out_of_range()).

bounded_integer_accepts_in_range_test()  -> ?PROP(prop_bounded_integer_accepts_in_range()).
bounded_integer_rejects_below_min_test() -> ?PROP(prop_bounded_integer_rejects_below_min()).
bounded_integer_rejects_above_max_test() -> ?PROP(prop_bounded_integer_rejects_above_max()).

non_negative_shortcut_equivalence_test() -> ?PROP(prop_non_negative_equivalence()).
positive_integer_shortcut_equivalence_test() -> ?PROP(prop_positive_integer_equivalence()).

empty_constraints_equivalent_to_plain_integer_test() ->
    ?PROP(prop_empty_constraints_equivalent_to_plain_integer()).

string_and_integer_input_agree_test() ->
    ?PROP(prop_string_and_integer_input_agree()).

bounded_float_accepts_in_range_test()  -> ?PROP(prop_bounded_float_accepts_in_range()).
bounded_float_rejects_below_min_test() -> ?PROP(prop_bounded_float_rejects_below_min()).
bounded_float_rejects_above_max_test() -> ?PROP(prop_bounded_float_rejects_above_max()).

non_negative_float_shortcut_equivalence_test() ->
    ?PROP(prop_non_negative_float_equivalence()).
positive_float_shortcut_equivalence_test() ->
    ?PROP(prop_positive_float_equivalence()).

percent_accepts_zero_to_hundred_test() -> ?PROP(prop_percent_accepts_zero_to_hundred()).
percent_rejects_out_of_range_test()    -> ?PROP(prop_percent_rejects_out_of_range()).

integer_gt_lt_correctness_test() -> ?PROP(prop_integer_gt_lt_correctness()).


%% Properties

prop_port_round_trip() ->
    ?FORALL(N, range(0, 65535),
        N =:= from_string(N, port) andalso
        N =:= from_string(integer_to_list(N), port)).

prop_port_rejects_out_of_range() ->
    ?FORALL(N, oneof([range(-1000, -1), range(65536, 100000)]),
        case from_string(N, port) of
            {error, {range_violation, {N, {min, 0}}}}     -> true;
            {error, {range_violation, {N, {max, 65535}}}} -> true;
            _ -> false
        end).

prop_byte_round_trip() ->
    ?FORALL(N, range(0, 255),
        N =:= from_string(N, byte)).

prop_byte_rejects_out_of_range() ->
    ?FORALL(N, oneof([range(-1000, -1), range(256, 100000)]),
        case from_string(N, byte) of
            {error, {range_violation, {N, {min, 0}}}}   -> true;
            {error, {range_violation, {N, {max, 255}}}} -> true;
            _ -> false
        end).

prop_bounded_integer_accepts_in_range() ->
    ?FORALL({Min, Max}, valid_range(),
        ?FORALL(N, range(Min, Max),
            N =:= from_string(N, {integer, [{min, Min}, {max, Max}]}))).

prop_bounded_integer_rejects_below_min() ->
    ?FORALL({Min, Max}, valid_range(),
        ?FORALL(N, integer_below(Min),
            case from_string(N, {integer, [{min, Min}, {max, Max}]}) of
                {error, {range_violation, {N, {min, Min}}}} -> true;
                _ -> false
            end)).

prop_bounded_integer_rejects_above_max() ->
    ?FORALL({Min, Max}, valid_range(),
        ?FORALL(N, integer_above(Max),
            case from_string(N, {integer, [{min, Min}, {max, Max}]}) of
                {error, {range_violation, {N, {max, Max}}}} -> true;
                _ -> false
            end)).

prop_non_negative_equivalence() ->
    ?FORALL(N, integer(),
        from_string(N, {integer, non_negative}) =:=
        from_string(N, {integer, [{min, 0}]})).

prop_positive_integer_equivalence() ->
    ?FORALL(N, integer(),
        from_string(N, {integer, positive}) =:=
        from_string(N, {integer, [{min, 1}]})).

prop_empty_constraints_equivalent_to_plain_integer() ->
    ?FORALL(N, integer(),
        from_string(N, integer) =:= from_string(N, {integer, []})).

prop_string_and_integer_input_agree() ->
    ?FORALL({N, Min, Max}, {integer(), integer(), integer()},
        begin
            T = {integer, [{min, Min}, {max, Max}]},
            from_string(N, T) =:= from_string(integer_to_list(N), T)
        end).

prop_bounded_float_accepts_in_range() ->
    ?FORALL({Min, Max}, valid_float_range(),
        ?FORALL(F, float_in(Min, Max),
            F =:= from_string(F, {float, [{min, Min}, {max, Max}]}))).

prop_bounded_float_rejects_below_min() ->
    ?FORALL({Min, Max}, valid_float_range(),
        ?FORALL(F, float_below(Min),
            case from_string(F, {float, [{min, Min}, {max, Max}]}) of
                {error, {range_violation, {F, {min, Min}}}} -> true;
                _ -> false
            end)).

prop_bounded_float_rejects_above_max() ->
    ?FORALL({Min, Max}, valid_float_range(),
        ?FORALL(F, float_above(Max),
            case from_string(F, {float, [{min, Min}, {max, Max}]}) of
                {error, {range_violation, {F, {max, Max}}}} -> true;
                _ -> false
            end)).

prop_non_negative_float_equivalence() ->
    ?FORALL(F, float(),
        from_string(F, {float, non_negative}) =:=
        from_string(F, {float, [{min, +0.0}]})).

prop_positive_float_equivalence() ->
    ?FORALL(F, float(),
        from_string(F, {float, positive}) =:=
        from_string(F, {float, [{gt, +0.0}]})).

prop_percent_accepts_zero_to_hundred() ->
    ?FORALL(N, range(0, 100),
        N =:= from_string(N, percent) andalso
        N =:= from_string(integer_to_list(N) ++ "%", percent)).

prop_percent_rejects_out_of_range() ->
    ?FORALL(N, oneof([range(-1000, -1), range(101, 1000)]),
        case from_string(N, percent) of
            {error, {range, _}} -> true;
            _ -> false
        end).

prop_integer_gt_lt_correctness() ->
    ?FORALL({N, Bound}, {integer(), integer()},
        begin
            GtResult = from_string(N, {integer, [{gt, Bound}]}),
            LtResult = from_string(N, {integer, [{lt, Bound}]}),
            GtOk = case N > Bound of
                       true  -> GtResult =:= N;
                       false -> match_range_violation(GtResult, N, {gt, Bound})
                   end,
            LtOk = case N < Bound of
                       true  -> LtResult =:= N;
                       false -> match_range_violation(LtResult, N, {lt, Bound})
                   end,
            GtOk andalso LtOk
        end).

match_range_violation({error, {range_violation, {V, C}}}, V, C) -> true;
match_range_violation(_, _, _) -> false.


%% Generators

valid_range() ->
    ?LET({A, B}, {integer(), integer()},
         case A =< B of
             true  -> {A, B};
             false -> {B, A}
         end).

integer_below(Min) ->
    ?LET(D, pos_integer(), Min - D).

integer_above(Max) ->
    ?LET(D, pos_integer(), Max + D).

%% Generate {Min, Max} with a guaranteed positive gap so the
%% in-range/out-of-range generators below can always produce a value.
valid_float_range() ->
    ?LET({A, B, Gap}, {float(), float(), ?SUCHTHAT(G, float(), G > 0.0)},
         begin
             Lo = min(A, B),
             Hi = max(A, B),
             {Lo, Hi + Gap}
         end).

float_in(Min, Max) ->
    %% Clamp the interpolated value back into [Min, Max] in case
    %% floating-point arithmetic drifts past either endpoint.
    ?LET(P, choose(0, 1000),
         max(Min, min(Max, Min + (Max - Min) * (P / 1000.0)))).

float_below(Min) ->
    ?LET(D, ?SUCHTHAT(F, float(), F > 0.0),
         Min - D).

float_above(Max) ->
    ?LET(D, ?SUCHTHAT(F, float(), F > 0.0),
         Max + D).
