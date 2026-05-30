-module(cuttlefish_uri_marker_tests).

-include_lib("eunit/include/eunit.hrl").

%% The `"uri"' marker validator ships as a no-op, so a schema that
%% historically declared an inline no-op `"uri"' validator can drop
%% the local definition without rewriting call sites.

uri_marker_accepts_anything_test() ->
    Schema =
        "{mapping, \"u\", \"app.u\","
        "  [{datatype, string}, {validators, [\"uri\"]}]}.\n",
    {T, M, V} = cuttlefish_schema:strings([Schema]),
    %% Template-style values that the strict `uri' datatype would
    %% reject still pass the marker.
    Conf = [{["u"], "https://{{node}}/health"}],
    ?assertEqual([{app, [{u, "https://{{node}}/health"}]}],
                 cuttlefish_generator:map({T, M, V}, Conf)).

uri_marker_accepts_empty_string_test() ->
    Schema =
        "{mapping, \"u\", \"app.u\","
        "  [{datatype, string}, {validators, [\"uri\"]}]}.\n",
    {T, M, V} = cuttlefish_schema:strings([Schema]),
    ?assertEqual([{app, [{u, ""}]}],
                 cuttlefish_generator:map({T, M, V}, [{["u"], ""}])).

user_defined_uri_validator_wins_test() ->
    %% A user-defined `"uri"' that's actually strict beats the marker.
    Schema =
        "{validator, \"uri\", \"must start with https://\","
        "  fun(S) -> case S of"
        "    \"https://\" ++ _ -> true;"
        "    _ -> false"
        "  end end}.\n"
        "{mapping, \"u\", \"app.u\","
        "  [{datatype, string}, {validators, [\"uri\"]}]}.\n",
    {T, M, V} = cuttlefish_schema:strings([Schema]),
    ?assertMatch({error, validation, _},
                 cuttlefish_generator:map({T, M, V},
                                          [{["u"], "http://example.com"}])).
