-module(cuttlefish_uri_datatype_tests).

-include_lib("eunit/include/eunit.hrl").

-define(XLATE(X), lists:flatten(cuttlefish_error:xlate(X))).

bare_uri_accepts_http_test() ->
    ?assertEqual("http://example.com",
                 cuttlefish_datatypes:from_string("http://example.com", uri)).

bare_uri_accepts_https_test() ->
    ?assertEqual("https://example.com",
                 cuttlefish_datatypes:from_string("https://example.com", uri)).

bare_uri_accepts_port_test() ->
    ?assertEqual("https://example.com:8443",
                 cuttlefish_datatypes:from_string("https://example.com:8443", uri)).

bare_uri_accepts_path_test() ->
    ?assertEqual("https://example.com/.well-known/jwks.json",
                 cuttlefish_datatypes:from_string(
                   "https://example.com/.well-known/jwks.json", uri)).

bare_uri_accepts_query_string_test() ->
    ?assertEqual("https://example.com/path?foo=1&bar=2",
                 cuttlefish_datatypes:from_string(
                   "https://example.com/path?foo=1&bar=2", uri)).

bare_uri_accepts_fragment_test() ->
    ?assertEqual("https://example.com/path#section",
                 cuttlefish_datatypes:from_string(
                   "https://example.com/path#section", uri)).

bare_uri_accepts_trailing_slash_test() ->
    ?assertEqual("https://example.com/",
                 cuttlefish_datatypes:from_string("https://example.com/", uri)).

bare_uri_accepts_userinfo_test() ->
    ?assertEqual("https://user:pass@example.com",
                 cuttlefish_datatypes:from_string(
                   "https://user:pass@example.com", uri)).

bare_uri_accepts_ipv4_host_test() ->
    ?assertEqual("http://192.168.1.10:8080",
                 cuttlefish_datatypes:from_string("http://192.168.1.10:8080", uri)).

bare_uri_accepts_ipv6_host_test() ->
    ?assertEqual("https://[::1]:8443",
                 cuttlefish_datatypes:from_string("https://[::1]:8443", uri)).

bare_uri_accepts_localhost_test() ->
    ?assertEqual("http://localhost:5000",
                 cuttlefish_datatypes:from_string("http://localhost:5000", uri)).

bare_uri_preserves_uppercase_scheme_test() ->
    ?assertEqual("HTTPS://example.com",
                 cuttlefish_datatypes:from_string("HTTPS://example.com", uri)).

bare_uri_trims_whitespace_test() ->
    ?assertEqual("https://example.com",
                 cuttlefish_datatypes:from_string("  https://example.com  ", uri)).

reject_empty_test() ->
    ?assertMatch({error, {uri_empty, _}},
                 cuttlefish_datatypes:from_string("", uri)),
    ?assertMatch({error, {uri_empty, _}},
                 cuttlefish_datatypes:from_string("   ", uri)).

reject_no_scheme_test() ->
    ?assertMatch({error, {uri_no_scheme, _}},
                 cuttlefish_datatypes:from_string("example.com/path", uri)).

reject_no_host_test() ->
    ?assertMatch({error, {uri_no_host, _}},
                 cuttlefish_datatypes:from_string("https://", uri)),
    ?assertMatch({error, {uri_no_host, _}},
                 cuttlefish_datatypes:from_string("https:///foo", uri)).

reject_wrong_scheme_test() ->
    ?assertMatch({error, {uri_bad_scheme, _}},
                 cuttlefish_datatypes:from_string("ftp://example.com", uri)),
    ?assertMatch({error, {uri_bad_scheme, _}},
                 cuttlefish_datatypes:from_string("file:///etc/passwd", uri)).

reject_malformed_test() ->
    ?assertMatch({error, {uri_malformed, _}},
                 cuttlefish_datatypes:from_string("http://example.com:abc", uri)).

reject_scheme_relative_test() ->
    ?assertMatch({error, {uri_no_scheme, _}},
                 cuttlefish_datatypes:from_string("//example.com/foo", uri)).

reject_scheme_only_test() ->
    ?assertMatch({error, {uri_no_host, _}},
                 cuttlefish_datatypes:from_string("https:", uri)).

reject_mixed_scheme_list_test() ->
    ?assertMatch({error, {uri_schemes_invalid, _}},
                 cuttlefish_datatypes:from_string("https://example.com",
                                                  {uri, [http, "https"]})).

https_only_accepts_https_test() ->
    ?assertEqual("https://example.com",
                 cuttlefish_datatypes:from_string("https://example.com",
                                                  {uri, [https]})).

https_only_rejects_http_test() ->
    Result = cuttlefish_datatypes:from_string("http://example.com", {uri, [https]}),
    ?assertMatch({error, {uri_bad_scheme, _}}, Result),
    {error, Detail} = Result,
    Msg = ?XLATE(Detail),
    ?assert(string:find(Msg, "https") =/= nomatch).

custom_schemes_amqp_test() ->
    ?assertEqual("amqps://broker.local",
                 cuttlefish_datatypes:from_string("amqps://broker.local",
                                                  {uri, [amqp, amqps]})).

empty_scheme_list_rejected_test() ->
    ?assertMatch({error, {uri_schemes_empty, _}},
                 cuttlefish_datatypes:from_string("https://example.com",
                                                  {uri, []})).

scheme_list_with_non_atom_rejected_test() ->
    ?assertMatch({error, {uri_schemes_invalid, _}},
                 cuttlefish_datatypes:from_string("https://example.com",
                                                  {uri, ["https"]})).

to_string_is_identity_test() ->
    ?assertEqual("https://example.com",
                 cuttlefish_datatypes:to_string("https://example.com", uri)),
    ?assertEqual("https://example.com",
                 cuttlefish_datatypes:to_string("https://example.com", {uri, [https]})).

is_supported_test() ->
    ?assert(cuttlefish_datatypes:is_supported(uri)),
    ?assert(cuttlefish_datatypes:is_supported({uri, [https]})),
    ?assert(cuttlefish_datatypes:is_supported({uri, [http, https]})),
    ?assert(cuttlefish_datatypes:is_supported({uri, [amqp, amqps]})),
    ?assertNot(cuttlefish_datatypes:is_supported({uri, []})),
    ?assertNot(cuttlefish_datatypes:is_supported({uri, ["http"]})),
    ?assertNot(cuttlefish_datatypes:is_supported({uri, foo})).

list_of_uris_supported_test() ->
    ?assert(cuttlefish_datatypes:is_supported({list, uri})),
    ?assert(cuttlefish_datatypes:is_supported({list, {uri, [https]}})).

error_messages_test() ->
    Cases = [
        {{uri_empty, ""},                      "URI value cannot be empty"},
        {{uri_no_scheme, "example.com"},       "scheme"},
        {{uri_no_host, "https://"},            "host"},
        {{uri_malformed, "http://x:abc"},      "parseable"},
        {{uri_schemes_empty, []},              "scheme list"},
        {{uri_bad_scheme, {"ftp://x", "ftp", [http, https]}}, "expected one of"}
    ],
    lists:foreach(fun({Err, Substring}) ->
        Msg = ?XLATE(Err),
        ?assertNotEqual([], Msg),
        ?assert(string:find(Msg, Substring) =/= nomatch,
                io_lib:format("expected ~p in message ~p", [Substring, Msg]))
    end, Cases).
