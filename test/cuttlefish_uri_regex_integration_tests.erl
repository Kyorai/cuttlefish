-module(cuttlefish_uri_regex_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%% Exercises the uri and regex datatypes through the full generator pipeline.

uri_mapping_accepts_https_test() ->
    Mappings = [
        cuttlefish_mapping:parse({mapping, "auth.jwks_url",
                                  "myapp.jwks_url",
                                  [{datatype, {uri, [https]}}]})
    ],
    Conf = [{["auth", "jwks_url"], "https://identity.example.com/jwks"}],
    Result = cuttlefish_generator:map({[], Mappings, []}, Conf),
    ?assertEqual([{myapp, [{jwks_url, "https://identity.example.com/jwks"}]}],
                 Result).

uri_mapping_rejects_http_when_only_https_allowed_test() ->
    Mappings = [
        cuttlefish_mapping:parse({mapping, "auth.jwks_url",
                                  "myapp.jwks_url",
                                  [{datatype, {uri, [https]}}]})
    ],
    Conf = [{["auth", "jwks_url"], "http://identity.example.com/jwks"}],
    Result = cuttlefish_generator:map({[], Mappings, []}, Conf),
    ?assertMatch({error, transform_datatypes, _}, Result).

regex_mapping_accepts_safe_pattern_test() ->
    Mappings = [
        cuttlefish_mapping:parse({mapping, "vhost.pattern",
                                  "myapp.vhost_pattern",
                                  [{datatype, regex}]})
    ],
    Conf = [{["vhost", "pattern"], "^[a-z0-9_-]+$"}],
    Result = cuttlefish_generator:map({[], Mappings, []}, Conf),
    ?assertEqual([{myapp, [{vhost_pattern, "^[a-z0-9_-]+$"}]}], Result).

regex_mapping_rejects_excessive_backtracking_test() ->
    Mappings = [
        cuttlefish_mapping:parse({mapping, "vhost.pattern",
                                  "myapp.vhost_pattern",
                                  [{datatype, regex}]})
    ],
    Conf = [{["vhost", "pattern"], "^([a-z]+)+$"}],
    Result = cuttlefish_generator:map({[], Mappings, []}, Conf),
    ?assertMatch({error, transform_datatypes, _}, Result).

list_of_uris_test() ->
    Mappings = [
        cuttlefish_mapping:parse({mapping, "endpoints",
                                  "myapp.endpoints",
                                  [{datatype, {list, uri}}]})
    ],
    Conf = [{["endpoints"], "https://a.example.com, http://b.example.com"}],
    Result = cuttlefish_generator:map({[], Mappings, []}, Conf),
    ?assertEqual([{myapp, [{endpoints,
                            ["https://a.example.com", "http://b.example.com"]}]}],
                 Result).

list_of_regexes_test() ->
    Mappings = [
        cuttlefish_mapping:parse({mapping, "patterns",
                                  "myapp.patterns",
                                  [{datatype, {list, regex}}]})
    ],
    Conf = [{["patterns"], "^foo$, ^bar.*$"}],
    Result = cuttlefish_generator:map({[], Mappings, []}, Conf),
    ?assertEqual([{myapp, [{patterns, ["^foo$", "^bar.*$"]}]}], Result).
