-module(cuttlefish_constraint_features_proper_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PROP(P), ?assert(proper:quickcheck(P, [{numtests, 200}, {to_file, user}]))).

%% Property-based coverage for `allow_infinity', bytesize and
%% duration range constraints, constraint-list validators, and the
%% normalised diff renderer.

allow_infinity_idempotent_test() -> ?PROP(prop_allow_infinity_idempotent()).
allow_infinity_numeric_path_test() -> ?PROP(prop_allow_infinity_numeric_path()).
bytesize_constraint_round_trip_test() -> ?PROP(prop_bytesize_constraint()).
duration_constraint_round_trip_test() -> ?PROP(prop_duration_constraint()).
constraint_validator_first_failure_wins_test() ->
    ?PROP(prop_first_failure_wins()).
render_normalised_stable_under_permutation_test() ->
    ?PROP(prop_render_stable_under_permutation()).

%% For any integer N and any constraint set that includes
%% `allow_infinity', parsing `"infinity"' yields the atom `infinity'.
prop_allow_infinity_idempotent() ->
    ?FORALL(Extra, list(constraint_atom()),
        cuttlefish_datatypes:from_string("infinity",
            {integer, [allow_infinity | Extra]}) =:= infinity).

%% For a non-negative integer string `N', conversion through a
%% non_negative constraint with allow_infinity returns the same
%% integer that the bare integer datatype would return.
prop_allow_infinity_numeric_path() ->
    ?FORALL(N, non_neg_integer(),
        cuttlefish_datatypes:from_string(integer_to_list(N),
            {integer, [non_negative, allow_infinity]}) =:= N).

%% A bytesize value in a wide range round-trips: parse, ensure the
%% parsed value falls inside `[Min, Max]'.
prop_bytesize_constraint() ->
    ?FORALL(N, choose(0, 1073741824),
        begin
            Parsed = cuttlefish_datatypes:from_string(
                       integer_to_list(N),
                       {bytesize, [{min, 0}, {max, 1073741824}]}),
            is_integer(Parsed) andalso Parsed =:= N
        end).

%% A duration in milliseconds round-trips with bounds.
prop_duration_constraint() ->
    ?FORALL(N, choose(0, 60000),
        begin
            Parsed = cuttlefish_datatypes:from_string(
                       integer_to_list(N) ++ "ms",
                       {duration, ms, [{min, 0}, {max, 60000}]}),
            is_integer(Parsed) andalso Parsed >= 0 andalso Parsed =< 60000
        end).

%% Two constraints — first rejects all, second accepts all — must
%% report failure attributable to the first one (constraint order is
%% left-to-right).
prop_first_failure_wins() ->
    ?FORALL(N, non_neg_integer(),
        begin
            DT = {integer, [{validator, fun(_) -> false end},
                            {validator, fun(_) -> true end}]},
            Result = cuttlefish_datatypes:from_string(integer_to_list(N), DT),
            case Result of
                {error, {constraint_validator_failed, _}} -> true;
                _ -> false
            end
        end).

%% Permuting the order of `{app, ...}' top-level entries and the
%% order of inner key/value pairs does not change the rendered form.
prop_render_stable_under_permutation() ->
    ?FORALL({Keys, Values}, {non_empty(list(atom_key())),
                              non_empty(list(simple_value()))},
        begin
            %% Build a small but non-empty proplist
            Pairs = lists:zip(
                      lists:sublist(Keys, length(Values)),
                      lists:sublist(Values, length(Keys))),
            %% Drop duplicate keys
            Unique = lists:ukeysort(1, Pairs),
            Config = [{app, Unique}],
            Shuffled = [{app, lists:reverse(Unique)}],
            A = lists:flatten(cuttlefish_diff:render_normalised(Config)),
            B = lists:flatten(cuttlefish_diff:render_normalised(Shuffled)),
            A =:= B
        end).

%%% Generators

constraint_atom() ->
    oneof([non_negative, positive]).

atom_key() ->
    elements([alpha, beta, gamma, delta, epsilon, zeta]).

simple_value() ->
    oneof([integer(), atom(), boolean()]).
