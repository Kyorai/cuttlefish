-module(cuttlefish_partial_inline_tests).

-include_lib("eunit/include/eunit.hrl").

%% `cuttlefish_partial:bind_translation/3` is a public helper; a
%% schema author can call it from inside an inline `{translation,
%% ...}` to use the same prefix-binding ergonomics partials provide
%% without authoring a `.partial' file.

bind_translation_inline_from_schema_test() ->
    Schema =
        "{mapping, \"x.versions.$v\", \"y.versions\", [{datatype, atom}]}.\n"
        "{translation, \"y.versions\","
        "    cuttlefish_partial:bind_translation("
        "      fun(Conf, ConfPrefix, _AppPrefix) ->"
        "          [V || {_, V} <- cuttlefish_variable:filter_by_prefix("
        "                              ConfPrefix ++ \".versions\", Conf)]"
        "      end, \"x\", \"y\")}.\n",
    SchemaTuple = cuttlefish_schema:strings([Schema]),
    Conf = [{["x", "versions", "tlsv1.3"], 'tlsv1.3'},
            {["x", "versions", "tlsv1.2"], 'tlsv1.2'}],
    Config = cuttlefish_generator:map(SchemaTuple, Conf),
    Versions = proplists:get_value(versions, proplists:get_value(y, Config)),
    ?assertEqual(lists:sort(['tlsv1.2', 'tlsv1.3']), lists:sort(Versions)).

bind_translation_inline_reuses_same_fun_shape_twice_test() ->
    %% A consumer with two contexts can bind the same fun shape
    %% twice with different prefixes.
    Schema =
        "{mapping, \"a.versions.$v\", \"app_a.versions\", [{datatype, atom}]}.\n"
        "{mapping, \"b.versions.$v\", \"app_b.versions\", [{datatype, atom}]}.\n"
        "{translation, \"app_a.versions\","
        "    cuttlefish_partial:bind_translation("
        "      fun(Conf, P, _A) ->"
        "          [V || {_, V} <- cuttlefish_variable:filter_by_prefix("
        "                              P ++ \".versions\", Conf)]"
        "      end, \"a\", \"app_a\")}.\n"
        "{translation, \"app_b.versions\","
        "    cuttlefish_partial:bind_translation("
        "      fun(Conf, P, _A) ->"
        "          [V || {_, V} <- cuttlefish_variable:filter_by_prefix("
        "                              P ++ \".versions\", Conf)]"
        "      end, \"b\", \"app_b\")}.\n",
    SchemaTuple = cuttlefish_schema:strings([Schema]),
    Conf = [{["a", "versions", "tlsv1.3"], 'tlsv1.3'},
            {["b", "versions", "tlsv1.2"], 'tlsv1.2'}],
    Config = cuttlefish_generator:map(SchemaTuple, Conf),
    ?assertEqual(['tlsv1.3'],
                 proplists:get_value(versions, proplists:get_value(app_a, Config))),
    ?assertEqual(['tlsv1.2'],
                 proplists:get_value(versions, proplists:get_value(app_b, Config))).
