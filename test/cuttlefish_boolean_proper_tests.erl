-module(cuttlefish_boolean_proper_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PROP(P), ?assert(proper:quickcheck(P, [{numtests, 300}, {to_file, user}]))).

canonical_atom_roundtrip_test()    -> ?PROP(prop_canonical_atom_roundtrip()).
canonical_string_roundtrip_test()  -> ?PROP(prop_canonical_string_roundtrip()).
to_string_yields_canonical_test()  -> ?PROP(prop_to_string_yields_canonical()).
unknown_strings_rejected_test()    -> ?PROP(prop_unknown_strings_rejected()).
unknown_atoms_rejected_test()      -> ?PROP(prop_unknown_atoms_rejected()).
list_of_boolean_parses_test()      -> ?PROP(prop_list_of_boolean_parses()).
atoms_are_fixed_points_test()      -> ?PROP(prop_atoms_are_fixed_points()).
from_string_is_total_test()        -> ?PROP(prop_from_string_is_total()).

%% Round-trip from an Erlang boolean: atom -> string -> atom.
prop_canonical_atom_roundtrip() ->
    ?FORALL(B, boolean(),
        B =:= cuttlefish_datatypes:from_string(
                cuttlefish_datatypes:to_string(B, boolean), boolean)).

%% Round-trip from a canonical string: string -> atom -> string.
prop_canonical_string_roundtrip() ->
    ?FORALL(S, oneof(["true", "false"]),
        S =:= cuttlefish_datatypes:to_string(
                cuttlefish_datatypes:from_string(S, boolean), boolean)).

%% `to_string/2` on any accepted input yields one of the canonical strings.
prop_to_string_yields_canonical() ->
    ?FORALL(V, accepted_value(),
        lists:member(cuttlefish_datatypes:to_string(V, boolean),
                     ["true", "false"])).

%% Any string other than `"true"` or `"false"` is rejected with a
%% `{conversion, ...}` error.
prop_unknown_strings_rejected() ->
    ?FORALL(S, non_canonical_string(),
        case cuttlefish_datatypes:from_string(S, boolean) of
            {error, {conversion, {S, boolean}}} -> true;
            _                                   -> false
        end).

%% Any atom other than `true` or `false` is rejected; the error payload
%% preserves the original atom for diagnostics.
prop_unknown_atoms_rejected() ->
    ?FORALL(A, non_canonical_atom(),
        case cuttlefish_datatypes:from_string(A, boolean) of
            {error, {conversion, {A, boolean}}} -> true;
            _                                   -> false
        end).

%% A comma-separated list of canonical strings parses to the matching list
%% of Erlang booleans.
prop_list_of_boolean_parses() ->
    ?FORALL(Bs, non_empty(list(boolean())),
        begin
            Strings = [atom_to_list(B) || B <- Bs],
            Bs =:= cuttlefish_datatypes:from_string(
                     string:join(Strings, ","), {list, boolean})
        end).

%% The atoms `true` and `false` are fixed points of `from_string/2`.
prop_atoms_are_fixed_points() ->
    ?FORALL(B, boolean(),
        B =:= cuttlefish_datatypes:from_string(B, boolean)).

%% `from_string/2` always returns either a boolean atom or a `{conversion, ...}` tuple.
prop_from_string_is_total() ->
    ?FORALL(X, oneof([true, false, "true", "false",
                      non_canonical_string(), non_canonical_atom()]),
        case cuttlefish_datatypes:from_string(X, boolean) of
            true                          -> true;
            false                         -> true;
            {error, {conversion, {X, boolean}}} -> true;
            _                             -> false
        end).

%%
%% Generators
%%

accepted_value() ->
    oneof([true, false, "true", "false"]).

%% Lowercase, uppercase, digits and spaces. Uppercase coverage means the
%% property also pins down case-sensitivity.
non_canonical_string() ->
    ?SUCHTHAT(S,
              non_empty(list(oneof("abcdefghijklmnopqrstuvwxyz"
                                   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                   "0123456789 "))),
              S =/= "true" andalso S =/= "false").

non_canonical_atom() ->
    ?LET(S, non_canonical_string(), list_to_atom(S)).
