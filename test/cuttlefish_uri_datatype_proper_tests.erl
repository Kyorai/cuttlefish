-module(cuttlefish_uri_datatype_proper_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PROP(P), ?assert(proper:quickcheck(P, [{numtests, 300}, {to_file, user}]))).

http_or_https_accepted_test()    -> ?PROP(prop_http_or_https_accepted()).
output_equals_trimmed_test()     -> ?PROP(prop_output_equals_trimmed()).
unknown_scheme_rejected_test()   -> ?PROP(prop_unknown_scheme_rejected()).
allowlist_filters_schemes_test() -> ?PROP(prop_allowlist_filters_schemes()).
trim_idempotent_test()           -> ?PROP(prop_trim_idempotent()).

prop_http_or_https_accepted() ->
    ?FORALL(URI, gen_uri([http, https]),
        is_list(cuttlefish_datatypes:from_string(URI, uri))).

prop_output_equals_trimmed() ->
    ?FORALL({Pre, Post, URI}, {gen_whitespace(), gen_whitespace(), gen_uri([http, https])},
        begin
            Padded = Pre ++ URI ++ Post,
            cuttlefish_datatypes:from_string(Padded, uri) =:= URI
        end).

prop_unknown_scheme_rejected() ->
    ?FORALL(URI, gen_uri([ftp]),
        case cuttlefish_datatypes:from_string(URI, {uri, [http, https]}) of
            {error, {uri_bad_scheme, _}} -> true;
            _ -> false
        end).

prop_allowlist_filters_schemes() ->
    ?FORALL({Allowed, AllowedURI, OtherURI}, gen_allowed_and_other(),
        begin
            AcceptedOK = is_list(cuttlefish_datatypes:from_string(
                                   AllowedURI, {uri, [Allowed]})),
            OtherRejected =
                case cuttlefish_datatypes:from_string(
                       OtherURI, {uri, [Allowed]}) of
                    {error, {uri_bad_scheme, _}} -> true;
                    _ -> false
                end,
            AcceptedOK andalso OtherRejected
        end).

gen_allowed_and_other() ->
    ?LET({A, B}, ?SUCHTHAT({X, Y}, {gen_scheme(), gen_scheme()}, X =/= Y),
         ?LET({UA, UB}, {gen_one_uri(A), gen_one_uri(B)}, {A, UA, UB})).

prop_trim_idempotent() ->
    ?FORALL(URI, gen_uri([http, https]),
        begin
            Once = cuttlefish_datatypes:from_string(URI, uri),
            Twice = cuttlefish_datatypes:from_string(Once, uri),
            Once =:= Twice
        end).

gen_uri(Schemes) ->
    ?LET(S, oneof(Schemes), gen_one_uri(S)).

gen_one_uri(Scheme) ->
    ?LET({Host, MaybePort, MaybePath},
         {gen_host(), gen_maybe_port(), gen_maybe_path()},
         atom_to_list(Scheme) ++ "://" ++ Host ++ MaybePort ++ MaybePath).

gen_scheme() -> oneof([http, https, ftp, amqp, amqps, ws, wss]).

gen_host() ->
    ?LET(Segments, non_empty(list(gen_label())),
         string:join(Segments, ".")).

gen_label() ->
    ?LET(Chars, non_empty(list(oneof("abcdefghijklmnopqrstuvwxyz0123456789"))),
         Chars).

gen_maybe_port() ->
    oneof(["", ":80", ":443", ":8080", ":65535"]).

gen_maybe_path() ->
    oneof(["", "/", "/foo", "/foo/bar", "/path?q=1", "/path#frag"]).

gen_whitespace() ->
    ?LET(N, integer(0, 4), string:copies(" ", N)).
