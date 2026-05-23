-module(cuttlefish_regex_datatype_proper_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PROP(P), ?assert(proper:quickcheck(P, [{numtests, 300}, {to_file, user}]))).

safe_patterns_accepted_test()      -> ?PROP(prop_safe_patterns_accepted()).
output_equals_input_test()         -> ?PROP(prop_output_equals_input()).
nested_quantifier_rejected_test()  -> ?PROP(prop_nested_quantifier_rejected()).
idempotent_on_success_test()       -> ?PROP(prop_idempotent_on_success()).

prop_safe_patterns_accepted() ->
    ?FORALL(Pat, gen_safe_pattern(),
        case cuttlefish_datatypes:from_string(Pat, regex) of
            S when is_list(S) -> true;
            _                 -> false
        end).

prop_output_equals_input() ->
    ?FORALL(Pat, gen_safe_pattern(),
        case cuttlefish_datatypes:from_string(Pat, regex) of
            S when is_list(S) -> S =:= Pat;
            _                 -> true
        end).

%% `.` is intentionally absent: it matches every probe character including the
%% trailing failure byte, so the engine never has to backtrack.
prop_nested_quantifier_rejected() ->
    ?FORALL(Class, oneof(["[a-z]", "[a-zA-Z]", "[a-z0-9]", "\\w", "\\d"]),
        begin
            Pat = "^(" ++ Class ++ "+)+$",
            case cuttlefish_datatypes:from_string(Pat, regex) of
                {error, {regex_excessive_backtracking, _}} -> true;
                _ -> false
            end
        end).

prop_idempotent_on_success() ->
    ?FORALL(Pat, gen_safe_pattern(),
        case cuttlefish_datatypes:from_string(Pat, regex) of
            S when is_list(S) ->
                S =:= cuttlefish_datatypes:from_string(S, regex);
            _ -> true
        end).

gen_safe_pattern() ->
    ?LET(Parts, non_empty(list(gen_pattern_atom())),
         maybe_anchor(lists:flatten(Parts))).

gen_pattern_atom() ->
    oneof([
        gen_literal_char(),
        gen_char_class(),
        ?LET({C, Q}, {gen_literal_char(), oneof([$?, $+, $*])}, [C, Q]),
        "\\."
    ]).

gen_literal_char() ->
    oneof("abcdefghijklmnopqrstuvwxyz0123456789").

gen_char_class() ->
    oneof(["[a-z]", "[0-9]", "[a-zA-Z]", "[a-z0-9]", "\\w", "\\d", "\\s"]).

maybe_anchor(Body) ->
    ?LET({Start, End}, {oneof(["", "^"]), oneof(["", "$"])},
         Start ++ Body ++ End).
