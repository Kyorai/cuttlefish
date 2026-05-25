-module(cuttlefish_boolean_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FROM(V), cuttlefish_datatypes:from_string(V, boolean)).
-define(TO(V),   cuttlefish_datatypes:to_string(V, boolean)).
-define(XLATE(X), lists:flatten(cuttlefish_error:xlate(X))).

is_supported_test() ->
    ?assert(cuttlefish_datatypes:is_supported(boolean)),
    ?assert(cuttlefish_datatypes:is_supported({list, boolean})).

from_string_accepts_canonical_strings_test() ->
    ?assertEqual(true,  ?FROM("true")),
    ?assertEqual(false, ?FROM("false")).

from_string_accepts_canonical_atoms_test() ->
    ?assertEqual(true,  ?FROM(true)),
    ?assertEqual(false, ?FROM(false)).

from_string_is_case_sensitive_test() ->
    ?assertMatch({error, {conversion, {"True",  boolean}}}, ?FROM("True")),
    ?assertMatch({error, {conversion, {"TRUE",  boolean}}}, ?FROM("TRUE")),
    ?assertMatch({error, {conversion, {"False", boolean}}}, ?FROM("False")).

from_string_rejects_flag_words_test() ->
    %% On/off/yes/no belong to `flag`, not `boolean`.
    ?assertMatch({error, {conversion, {"yes", boolean}}}, ?FROM("yes")),
    ?assertMatch({error, {conversion, {"no",  boolean}}}, ?FROM("no")),
    ?assertMatch({error, {conversion, {"on",  boolean}}}, ?FROM("on")),
    ?assertMatch({error, {conversion, {"off", boolean}}}, ?FROM("off")).

from_string_rejects_empty_and_whitespace_test() ->
    ?assertMatch({error, {conversion, {"",      boolean}}}, ?FROM("")),
    ?assertMatch({error, {conversion, {" true", boolean}}}, ?FROM(" true")),
    ?assertMatch({error, {conversion, {"true ", boolean}}}, ?FROM("true ")).

from_string_rejects_unknown_string_test() ->
    ?assertMatch({error, {conversion, {"perhaps", boolean}}}, ?FROM("perhaps")),
    ?assertMatch({error, {conversion, {"1",       boolean}}}, ?FROM("1")),
    ?assertMatch({error, {conversion, {"0",       boolean}}}, ?FROM("0")).

from_string_rejects_unknown_atom_test() ->
    ?assertMatch({error, {conversion, {perhaps, boolean}}}, ?FROM(perhaps)).

from_string_non_string_non_atom_falls_through_test() ->
    %% Wrong-shape input lands on the catch-all and reports `{type, ...}`.
    ?assertMatch({error, {type, {42, boolean}}},     ?FROM(42)),
    ?assertMatch({error, {type, {{a, b}, boolean}}}, ?FROM({a, b})).

to_string_canonical_atoms_test() ->
    ?assertEqual("true",  ?TO(true)),
    ?assertEqual("false", ?TO(false)).

to_string_canonical_strings_pass_through_test() ->
    ?assertEqual("true",  ?TO("true")),
    ?assertEqual("false", ?TO("false")).

to_string_non_canonical_falls_through_test() ->
    %% `to_string/2` has no clause for non-canonical input; it lands on the
    %% catch-all and reports `{type, ...}`.
    ?assertMatch({error, {type, {perhaps, boolean}}}, ?TO(perhaps)),
    ?assertMatch({error, {type, {"perhaps", boolean}}}, ?TO("perhaps")).

xlate_conversion_error_test() ->
    %% Pin the rendered error so any regression in
    %% `cuttlefish_error:xlate/1` fails visibly. `~tp` renders the string
    %% form with quotes and the atom form without.
    ?assertEqual("\"perhaps\" cannot be converted to a(n) boolean",
                 ?XLATE(?FROM("perhaps"))),
    ?assertEqual("perhaps cannot be converted to a(n) boolean",
                 ?XLATE(?FROM(perhaps))).

roundtrip_through_to_string_test() ->
    %% `to_string/2` inverts `from_string/2` on canonical values.
    ?assertEqual(true,  ?FROM(?TO(true))),
    ?assertEqual(false, ?FROM(?TO(false))).

list_of_boolean_test() ->
    ?assertEqual([true, false, true, false],
                 cuttlefish_datatypes:from_string("true, false, true, false",
                                                  {list, boolean})).

list_of_boolean_propagates_errors_test() ->
    %% Elements convert independently; bad ones surface inline.
    ?assertEqual([true,
                  {error, {conversion, {"perhaps", boolean}}},
                  false],
                 cuttlefish_datatypes:from_string("true, perhaps, false",
                                                  {list, boolean})).
