-module(cuttlefish_diff_tests).

-include_lib("eunit/include/eunit.hrl").

-import(cuttlefish_diff, [render_normalised/1, render_normalised/2]).

-define(LINES(IOData), string:split(lists:flatten(IOData), "\n", all)).

empty_config_renders_empty_test() ->
    ?assertEqual([], lists:flatten(render_normalised([]))).

single_app_section_renders_test() ->
    Out = lists:flatten(render_normalised([{app, [{key, "value"}]}])),
    ?assert(string:find(Out, "app =>") =/= nomatch),
    ?assert(string:find(Out, "key = \"value\"") =/= nomatch).

%% Two equivalent configs that differ only by key order produce the
%% same rendered output.
key_order_does_not_affect_output_test() ->
    A = [{app, [{a, 1}, {b, 2}, {c, 3}]}],
    B = [{app, [{c, 3}, {b, 2}, {a, 1}]}],
    ?assertEqual(lists:flatten(render_normalised(A)),
                 lists:flatten(render_normalised(B))).

%% Two configs that differ in a single value produce different
%% renderings.
value_change_visible_in_output_test() ->
    A = [{app, [{port, 5672}]}],
    B = [{app, [{port, 5673}]}],
    ?assertNotEqual(lists:flatten(render_normalised(A)),
                    lists:flatten(render_normalised(B))).

funs_are_stable_under_skip_funs_test() ->
    F1 = fun(X) -> X end,
    F2 = fun(Y) -> Y * 2 end,
    A = [{app, [{handler, F1}]}],
    B = [{app, [{handler, F2}]}],
    %% Different funs collapse to the same `#Fun<>' placeholder.
    ?assertEqual(lists:flatten(render_normalised(A, [{skip_funs, true}])),
                 lists:flatten(render_normalised(B, [{skip_funs, true}]))).

atom_quoting_default_is_loose_test() ->
    Out = lists:flatten(render_normalised([{app, [{key, hello}]}])),
    ?assert(string:find(Out, "key = hello") =/= nomatch).

atom_quoting_strict_quotes_special_atoms_test() ->
    %% An atom with a hyphen requires Erlang quoting; strict mode
    %% surfaces that, loose mode does not.
    Special = 'with-dashes',
    Strict = lists:flatten(render_normalised([{app, [{k, Special}]}],
                                             [{atom_quoting, strict}])),
    Loose  = lists:flatten(render_normalised([{app, [{k, Special}]}])),
    ?assert(string:find(Strict, "'with-dashes'") =/= nomatch),
    ?assert(string:find(Loose,  "with-dashes") =/= nomatch),
    ?assertEqual(nomatch, string:find(Loose, "'with-dashes'")).

binary_distinguished_from_string_test() ->
    Bin = lists:flatten(render_normalised([{app, [{k, <<"hi">>}]}])),
    Str = lists:flatten(render_normalised([{app, [{k, "hi"}]}])),
    ?assertNotEqual(Bin, Str).

deeply_nested_proplist_is_sorted_test() ->
    A = [{app, [{group, [{z, 1}, {a, 2}]}]}],
    B = [{app, [{group, [{a, 2}, {z, 1}]}]}],
    ?assertEqual(lists:flatten(render_normalised(A)),
                 lists:flatten(render_normalised(B))).

bare_atom_list_order_preserved_test() ->
    %% A list of atoms (e.g. TLS cipher preferences) is order-bearing.
    %% Two inputs with different orders must render differently.
    A = [{app, [{ciphers, [tls1, tls2, tls3]}]}],
    B = [{app, [{ciphers, [tls3, tls2, tls1]}]}],
    ?assertNotEqual(lists:flatten(render_normalised(A)),
                    lists:flatten(render_normalised(B))).

multiple_app_sections_render_in_sorted_order_test() ->
    %% Top-level entries are sorted alphabetically by app name,
    %% making the multi-app diff layout stable.
    A = [{zeta, [{k, 1}]}, {alpha, [{k, 2}]}, {gamma, [{k, 3}]}],
    Out = lists:flatten(render_normalised(A)),
    AlphaPos = string:str(Out, "alpha"),
    GammaPos = string:str(Out, "gamma"),
    ZetaPos  = string:str(Out, "zeta"),
    ?assert(AlphaPos > 0 andalso GammaPos > 0 andalso ZetaPos > 0),
    ?assert(AlphaPos < GammaPos),
    ?assert(GammaPos < ZetaPos).

skip_funs_false_emits_more_detail_test() ->
    %% With `{skip_funs, false}', a fun renders via `~p' instead of
    %% the fixed `#Fun<>' placeholder, so the output is longer.
    F = fun erlang:length/1,
    Stable = lists:flatten(render_normalised([{app, [{k, F}]}],
                                             [{skip_funs, true}])),
    Detailed = lists:flatten(render_normalised([{app, [{k, F}]}],
                                               [{skip_funs, false}])),
    ?assert(length(Detailed) > length(Stable)).

nested_map_value_renders_test() ->
    %% Maps land in app.config via translations and must render
    %% without crashing.
    Out = lists:flatten(render_normalised(
                          [{app, [{flags, #{a => 1, b => 2}}]}])),
    ?assert(string:find(Out, "#{") =/= nomatch),
    ?assert(string:find(Out, "=>") =/= nomatch).

real_pipeline_idempotent_under_render_test() ->
    %% Two runs of the same schema/conf produce identical output.
    Schema =
        "{mapping, \"x\", \"app.x\","
        "  [{datatype, integer}, {default, 7}]}.\n",
    {T, M, V} = cuttlefish_schema:strings([Schema]),
    Conf1 = cuttlefish_generator:map({T, M, V}, [{["x"], "42"}]),
    Conf2 = cuttlefish_generator:map({T, M, V}, [{["x"], "42"}]),
    ?assertEqual(lists:flatten(render_normalised(Conf1)),
                 lists:flatten(render_normalised(Conf2))).
