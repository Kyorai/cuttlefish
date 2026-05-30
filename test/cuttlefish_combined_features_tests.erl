-module(cuttlefish_combined_features_tests).

-include_lib("eunit/include/eunit.hrl").

%% End-to-end exercise of the constraint/validator/diff additions
%% working together. The schema below uses bytesize and duration
%% range constraints, an `allow_infinity' shortcut, a constraint-list
%% validator entry by name, a 5-tuple validator with aliases and a
%% deprecation hint, the built-in `byte' validator, and the built-in
%% `uri' marker. The result is then rendered with `cuttlefish_diff'.

every_feature_combined_test() ->
    Schema =
        "{validator, \"even\", \"even integer\","
        "  fun(N) when is_integer(N) -> N rem 2 =:= 0 end,"
        "  [{aliases, [\"even_legacy\"]}]}.\n"
        "{mapping, \"buffer.bytes\", \"app.buf\","
        "  [{datatype, {bytesize, [{min, 1}, {max, 1073741824}]}}]}.\n"
        "{mapping, \"poll.interval\", \"app.poll\","
        "  [{datatype, {duration, ms, [non_negative, allow_infinity]}}]}.\n"
        "{mapping, \"max_age\", \"app.max\","
        "  [{datatype, integer},"
        "   {validators, [\"byte\"]}]}.\n"
        "{mapping, \"webhook\", \"app.hook\","
        "  [{datatype, string}, {validators, [\"uri\"]}]}.\n"
        "{mapping, \"batch.size\", \"app.batch\","
        "  [{datatype, {integer, [{min, 2}, {validator, \"even_legacy\"}]}}]}.\n",
    {T, M, V} = cuttlefish_schema:strings([Schema]),
    Conf = [
        {["buffer", "bytes"], "10MB"},
        {["poll", "interval"], "infinity"},
        {["max_age"], "200"},
        {["webhook"], "https://{{node}}/notify"},
        {["batch", "size"], "8"}
    ],
    Result = cuttlefish_generator:map({T, M, V}, Conf),
    ?assertMatch([{app, _}], Result),
    [{app, AppCfg}] = Result,
    Sorted = lists:sort(AppCfg),
    ?assertEqual(8, proplists:get_value(batch, Sorted)),
    ?assertEqual(10485760, proplists:get_value(buf, Sorted)),
    ?assertEqual("https://{{node}}/notify", proplists:get_value(hook, Sorted)),
    ?assertEqual(200, proplists:get_value(max, Sorted)),
    ?assertEqual(infinity, proplists:get_value(poll, Sorted)),
    %% Idempotent rendering: two runs over the same input produce the
    %% same string.
    Out1 = lists:flatten(cuttlefish_diff:render_normalised(Result)),
    Out2 = lists:flatten(cuttlefish_diff:render_normalised(Result)),
    ?assertEqual(Out1, Out2).
