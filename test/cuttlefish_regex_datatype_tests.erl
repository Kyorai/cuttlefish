-module(cuttlefish_regex_datatype_tests).

-include_lib("eunit/include/eunit.hrl").

-define(XLATE(X), lists:flatten(cuttlefish_error:xlate(X))).

accepts_simple_anchored_test() ->
    ?assertEqual("^[a-z]+$",
                 cuttlefish_datatypes:from_string("^[a-z]+$", regex)).

accepts_dot_star_test() ->
    ?assertEqual(".*", cuttlefish_datatypes:from_string(".*", regex)).

accepts_alternation_test() ->
    ?assertEqual("^(foo|bar|baz)$",
                 cuttlefish_datatypes:from_string("^(foo|bar|baz)$", regex)).

accepts_word_class_test() ->
    ?assertEqual("\\w+",
                 cuttlefish_datatypes:from_string("\\w+", regex)).

accepts_non_capturing_group_test() ->
    ?assertEqual("(?:foo|bar)+",
                 cuttlefish_datatypes:from_string("(?:foo|bar)+", regex)).

accepts_atomic_group_test() ->
    %% Atomic groups do not backtrack.
    ?assertEqual("(?>a+)+b",
                 cuttlefish_datatypes:from_string("(?>a+)+b", regex)).

accepts_realistic_rabbitmq_pattern_test() ->
    Pattern = "^[a-zA-Z0-9_\\-./]+$",
    ?assertEqual(Pattern, cuttlefish_datatypes:from_string(Pattern, regex)).

accepts_pattern_with_anchors_test() ->
    ?assertEqual("^foo$",
                 cuttlefish_datatypes:from_string("^foo$", regex)).

rejects_empty_test() ->
    ?assertMatch({error, {regex_empty, _}},
                 cuttlefish_datatypes:from_string("", regex)).

rejects_unbalanced_bracket_test() ->
    ?assertMatch({error, {regex_invalid_syntax, _}},
                 cuttlefish_datatypes:from_string("[unclosed", regex)).

rejects_dangling_quantifier_test() ->
    ?assertMatch({error, {regex_invalid_syntax, _}},
                 cuttlefish_datatypes:from_string("*", regex)).

rejects_nested_quantifiers_test() ->
    ?assertMatch({error, {regex_excessive_backtracking, _}},
                 cuttlefish_datatypes:from_string("(a+)+", regex)).

rejects_anchored_nested_quantifiers_test() ->
    ?assertMatch({error, {regex_excessive_backtracking, _}},
                 cuttlefish_datatypes:from_string("^(a+)+$", regex)).

rejects_overlapping_alternation_test() ->
    ?assertMatch({error, {regex_excessive_backtracking, _}},
                 cuttlefish_datatypes:from_string("^(a|a)*$", regex)).

rejects_evil_email_test() ->
    Pattern = "^([a-zA-Z0-9_.+-]+)+@([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}$",
    ?assertMatch({error, {regex_excessive_backtracking, _}},
                 cuttlefish_datatypes:from_string(Pattern, regex)).

accepts_pcre_safe_pattern_test() ->
    %% PCRE 8.x detects the empty-match loop in (a*)*b and exits without
    %% backtracking, so the validator must not flag it.
    ?assertEqual("(a*)*b", cuttlefish_datatypes:from_string("(a*)*b", regex)).

rejects_word_class_backtracking_test() ->
    ?assertMatch({error, {regex_excessive_backtracking, _}},
                 cuttlefish_datatypes:from_string("^(\\w+)+$", regex)).

to_string_is_identity_test() ->
    ?assertEqual("^[a-z]+$",
                 cuttlefish_datatypes:to_string("^[a-z]+$", regex)).

is_supported_test() ->
    ?assert(cuttlefish_datatypes:is_supported(regex)),
    ?assert(cuttlefish_datatypes:is_supported({list, regex})).

error_messages_test() ->
    Cases = [
        {{regex_empty, ""},                                   "empty"},
        {{regex_invalid_syntax, {"[abc", "missing terminating ]", 4}}, "valid"},
        {{regex_excessive_backtracking, "(a+)+"},                              "backtracking"}
    ],
    lists:foreach(fun({Err, Substring}) ->
        Msg = ?XLATE(Err),
        ?assertNotEqual([], Msg),
        ?assert(string:find(Msg, Substring) =/= nomatch,
                io_lib:format("expected ~p in message ~p", [Substring, Msg]))
    end, Cases).
