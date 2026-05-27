-module(cuttlefish_partial_rabbit_ssl_options_tests).

-include_lib("eunit/include/eunit.hrl").

%% Real-world fidelity tests for the partial mechanism.
%%
%% The fixture at `test/fixtures/sample_app/priv/schema/rabbit_ssl_options.partial`
%% is a verbatim copy of the production partial shipped in
%% `deps/rabbit/priv/schema/ssl_options.partial` (rabbitmq-server).
%% The validators it references (`pem_file`, `byte`, `file_accessible`)
%% are normally defined in `rabbit.schema`; here they are injected via
%% `rabbit_validators_schema/0`. The `rabbit_cuttlefish` stub at
%% `test/fixtures/sample_app/src/rabbit_cuttlefish.erl` provides the
%% `optionally_tagged_binary/2` helper the partial's password
%% translation calls.

%% --- single-context happy path ------------------------------------

single_include_with_realistic_conf_produces_expected_app_config_test() ->
    setup(),
    Schema = compose(include_for("ssl_options", "rabbit.ssl_options")),
    SchemaTuple = cuttlefish_schema:strings(Schema),
    Conf = [
        {["ssl_options", "verify"], verify_peer},
        {["ssl_options", "fail_if_no_peer_cert"], true},
        {["ssl_options", "cacertfile"], "/etc/rabbitmq/ca.pem"},
        {["ssl_options", "certfile"], "/etc/rabbitmq/cert.pem"},
        {["ssl_options", "keyfile"], "/etc/rabbitmq/key.pem"},
        {["ssl_options", "depth"], 3},
        {["ssl_options", "versions", "tlsv1.3"], 'tlsv1.3'},
        {["ssl_options", "versions", "tlsv1.2"], 'tlsv1.2'}],
    Config = cuttlefish_generator:map(SchemaTuple, Conf),
    SslOpts = lookup(rabbit, ssl_options, Config),
    ?assertEqual(verify_peer, proplists:get_value(verify, SslOpts)),
    ?assertEqual(true, proplists:get_value(fail_if_no_peer_cert, SslOpts)),
    ?assertEqual("/etc/rabbitmq/ca.pem",
                 proplists:get_value(cacertfile, SslOpts)),
    ?assertEqual(3, proplists:get_value(depth, SslOpts)),
    Versions = proplists:get_value(versions, SslOpts),
    ?assertEqual(lists:sort(['tlsv1.2', 'tlsv1.3']),
                 lists:sort(Versions)).

%% --- multi-context (rabbit.schema's four-ssl_options pattern) ------

four_contexts_in_one_schema_produce_independent_app_configs_test() ->
    setup(),
    Schema = compose(
        include_for("ssl_options", "rabbit.ssl_options"),
        include_for("definitions.tls", "rabbit.definitions.ssl_options"),
        include_for("amqp_client.ssl_options", "amqp_client.ssl_options"),
        include_for("amqp10_client.ssl_options", "amqp10_client.ssl_options")),
    SchemaTuple = cuttlefish_schema:strings(Schema),
    Conf = [
        {["ssl_options", "verify"], verify_peer},
        {["definitions", "tls", "verify"], verify_none},
        {["amqp_client", "ssl_options", "depth"], 1},
        {["amqp10_client", "ssl_options", "depth"], 9}],
    Config = cuttlefish_generator:map(SchemaTuple, Conf),
    Rabbit = proplists:get_value(rabbit, Config),
    ?assertEqual(verify_peer,
                 proplists:get_value(verify,
                     proplists:get_value(ssl_options, Rabbit))),
    ?assertEqual(verify_none,
                 proplists:get_value(verify,
                     proplists:get_value(ssl_options,
                         proplists:get_value(definitions, Rabbit)))),
    ?assertEqual(1, proplists:get_value(depth,
                       proplists:get_value(ssl_options,
                           proplists:get_value(amqp_client, Config)))),
    ?assertEqual(9, proplists:get_value(depth,
                       proplists:get_value(ssl_options,
                           proplists:get_value(amqp10_client, Config)))).

%% --- override via merge (shovel-style password retype) -------------

consumer_overrides_password_datatype_via_merge_test() ->
    %% The merge changes the mapping's accepted datatype (string vs
    %% the partial's [tagged_binary, binary]). The bound translation
    %% from the partial still runs and converts the value to a
    %% binary for app config; overriding the output shape would
    %% require also redeclaring the translation.
    setup(),
    Schema = compose(
        include_for("shovel.ssl", "rabbitmq_shovel.ssl"),
        "{mapping, \"shovel.ssl.password\", \"rabbitmq_shovel.ssl.password\","
        " [merge, {datatype, string}]}.\n"),
    SchemaTuple = cuttlefish_schema:strings(Schema),
    Conf = [{["shovel", "ssl", "password"], "plaintext-pw"}],
    Config = cuttlefish_generator:map(SchemaTuple, Conf),
    SslOpts = lookup(rabbitmq_shovel, ssl, Config),
    ?assertEqual(<<"plaintext-pw">>, proplists:get_value(password, SslOpts)).

%% --- override via inline translation (custom versions handling) ----

consumer_replaces_versions_translation_test() ->
    setup(),
    Schema = compose(
        include_for("x.ssl", "y.ssl"),
        "{translation, \"y.ssl.versions\","
        " fun(_Conf) -> ['custom-version'] end}.\n"),
    SchemaTuple = cuttlefish_schema:strings(Schema),
    Conf = [{["x", "ssl", "versions", "tlsv1.3"], 'tlsv1.3'}],
    Config = cuttlefish_generator:map(SchemaTuple, Conf),
    ?assertEqual(['custom-version'],
                 proplists:get_value(versions,
                                     lookup(y, ssl, Config))).

%% --- exclude (drop a section the consumer doesn't expose) ----------

consumer_excludes_log_level_section_test() ->
    setup(),
    Schema = compose(
        "{include_partial, {sample_app, \"rabbit_ssl_options\"},"
        " [{prefix, \"x.ssl\"}, {app_prefix, \"y.ssl\"},"
        "  {exclude, [\"log_level\"]}]}.\n"),
    SchemaTuple = cuttlefish_schema:strings(Schema),
    {_T, Mappings, _V} = SchemaTuple,
    Vars = [cuttlefish_mapping:variable(M) || M <- Mappings],
    ?assertNot(lists:member(["x", "ssl", "log_level"], Vars)),
    ?assert(lists:member(["x", "ssl", "verify"], Vars)).

%% --- key.* translation correctly tokenises and binarises -----------

key_translation_assembles_tuple_correctly_test() ->
    setup(),
    Schema = compose(include_for("x.ssl", "y.ssl")),
    SchemaTuple = cuttlefish_schema:strings(Schema),
    Conf = [{["x", "ssl", "key", "RSAPrivateKey"], "PEM-BODY"}],
    Config = cuttlefish_generator:map(SchemaTuple, Conf),
    SslOpts = lookup(y, ssl, Config),
    ?assertEqual({'RSAPrivateKey', <<"PEM-BODY">>},
                 proplists:get_value(key, SslOpts)).

key_translation_works_at_any_prefix_depth_test() ->
    %% Robust against arbitrary include-site prefix depth, unlike the
    %% original rabbit.schema translation which hardcoded `[_, _, K]'.
    setup(),
    Cases = [
        {"deep.shovel.upstream.ssl", "rabbitmq_shovel.upstream.ssl",
         ["deep", "shovel", "upstream", "ssl", "key", "RSAPrivateKey"]},
        {"x.ssl", "y.ssl",
         ["x", "ssl", "key", "RSAPrivateKey"]},
        {"single", "single",
         ["single", "key", "RSAPrivateKey"]}],
    [begin
         Schema = compose(include_for(Prefix, AppPrefix)),
         SchemaTuple = cuttlefish_schema:strings(Schema),
         Conf = [{ConfKey, "PEM-AT-DEPTH"}],
         Config = cuttlefish_generator:map(SchemaTuple, Conf),
         [_ | Steps] = string:split(AppPrefix, ".", all),
         AppRoot = list_to_atom(hd(string:split(AppPrefix, ".", all))),
         AppCfg = proplists:get_value(AppRoot, Config, []),
         SslOpts = walk_proplist(Steps, AppCfg),
         ?assertEqual({'RSAPrivateKey', <<"PEM-AT-DEPTH">>},
                      proplists:get_value(key, SslOpts))
     end || {Prefix, AppPrefix, ConfKey} <- Cases].

%% --- ciphers ordering --------------------------------------------

ciphers_translation_preserves_reverse_order_test() ->
    setup(),
    Schema = compose(include_for("x.ssl", "y.ssl")),
    SchemaTuple = cuttlefish_schema:strings(Schema),
    Conf = [{["x", "ssl", "ciphers", "AAA"], "AAA"},
            {["x", "ssl", "ciphers", "BBB"], "BBB"},
            {["x", "ssl", "ciphers", "CCC"], "CCC"}],
    Config = cuttlefish_generator:map(SchemaTuple, Conf),
    Ciphers = proplists:get_value(ciphers, lookup(y, ssl, Config)),
    ?assertEqual(3, length(Ciphers)),
    ?assertEqual(["AAA", "BBB", "CCC"], lists:sort(Ciphers)).

%% --- empty conf doesn't invent values ----------------------------

empty_conf_does_not_invent_values_test() ->
    setup(),
    Schema = compose(include_for("x.ssl", "y.ssl")),
    SchemaTuple = cuttlefish_schema:strings(Schema),
    Config = cuttlefish_generator:map(SchemaTuple, []),
    YApp = proplists:get_value(y, Config, []),
    SslOpts = proplists:get_value(ssl, YApp, []),
    ?assertEqual(undefined, proplists:get_value(verify, SslOpts)),
    ?assertEqual(undefined, proplists:get_value(versions, SslOpts)).

%% --- password translation calls rabbit_cuttlefish stub -----------

password_translation_exercises_rabbit_cuttlefish_helper_test() ->
    setup(),
    Schema = compose(include_for("x.ssl", "y.ssl")),
    SchemaTuple = cuttlefish_schema:strings(Schema),
    Conf = [{["x", "ssl", "password"], "plain-pw"}],
    Config = cuttlefish_generator:map(SchemaTuple, Conf),
    SslOpts = lookup(y, ssl, Config),
    ?assertEqual(<<"plain-pw">>, proplists:get_value(password, SslOpts)).

%% ==================================================================
%% End-to-end embedded migration: real partial + multiple migrated
%% schema excerpts loaded together through `cuttlefish_schema:strings/1'.
%% Mirrors a non-trivial subset of the RabbitMQ tree to prove the
%% migration works when several plugins all pull in the shared partial
%% with different prefix pairs and a couple of consumer-specific
%% overrides.
%% ==================================================================

embedded_rabbitmq_migration_end_to_end_test() ->
    setup(),
    Schemas = [
        rabbit_validators_schema(),
        rabbit_primary_ssl_excerpt(),
        rabbit_definitions_tls_excerpt(),
        auth_backend_http_excerpt(),
        auth_backend_ldap_excerpt(),
        trust_store_excerpt(),
        peer_discovery_consul_excerpt()
    ],
    SchemaTuple = cuttlefish_schema:strings(Schemas),
    ?assertMatch({_T, _M, _V}, SchemaTuple),

    Conf = [
        %% rabbit primary listener TLS
        {["ssl_options", "verify"],     verify_peer},
        {["ssl_options", "cacertfile"], "/etc/rabbitmq/ca.pem"},
        {["ssl_options", "certfile"],   "/etc/rabbitmq/server.pem"},
        {["ssl_options", "keyfile"],    "/etc/rabbitmq/server.key"},
        {["ssl_options", "depth"],      2},
        {["ssl_options", "versions", "tlsv1.3"], 'tlsv1.3'},

        %% definitions HTTPS
        {["definitions", "tls", "verify"], verify_none},
        {["definitions", "tls", "cacertfile"], "/etc/rabbitmq/defs-ca.pem"},

        %% auth_backend_http
        {["auth_http", "ssl_options"], none},

        %% auth_backend_ldap
        {["auth_ldap", "ssl_options", "verify"], verify_peer},
        {["auth_ldap", "ssl_options", "cacertfile"], "/etc/ldap/ca.pem"},

        %% trust_store
        {["trust_store", "ssl_options", "verify"], verify_peer},

        %% peer_discovery_consul
        {["cluster_formation", "consul", "ssl_options", "verify"], verify_peer},
        {["cluster_formation", "consul", "ssl_options", "cacertfile"],
         "/etc/rabbitmq/consul-ca.pem"}
    ],

    Config = cuttlefish_generator:map(SchemaTuple, Conf),

    %% rabbit primary
    Primary = lookup(rabbit, ssl_options, Config),
    ?assertEqual(verify_peer, proplists:get_value(verify, Primary)),
    ?assertEqual("/etc/rabbitmq/ca.pem",
                 proplists:get_value(cacertfile, Primary)),
    ?assertEqual(2, proplists:get_value(depth, Primary)),
    ?assertEqual(['tlsv1.3'], proplists:get_value(versions, Primary)),

    %% rabbit definitions
    Defs = proplists:get_value(definitions,
                                proplists:get_value(rabbit, Config)),
    DefsSsl = proplists:get_value(ssl_options, Defs),
    ?assertEqual(verify_none, proplists:get_value(verify, DefsSsl)),
    ?assertEqual("/etc/rabbitmq/defs-ca.pem",
                 proplists:get_value(cacertfile, DefsSsl)),

    %% auth_backend_http: the `= none' shortcut disables TLS
    AuthHttp = proplists:get_value(rabbitmq_auth_backend_http, Config),
    ?assertEqual([], proplists:get_value(ssl_options, AuthHttp)),

    %% auth_backend_ldap: sub-options present
    AuthLdapSsl = lookup(rabbitmq_auth_backend_ldap, ssl_options, Config),
    ?assertEqual(verify_peer, proplists:get_value(verify, AuthLdapSsl)),
    ?assertEqual("/etc/ldap/ca.pem",
                 proplists:get_value(cacertfile, AuthLdapSsl)),

    %% trust_store
    TrustSsl = lookup(rabbitmq_trust_store, ssl_options, Config),
    ?assertEqual(verify_peer, proplists:get_value(verify, TrustSsl)),

    %% peer_discovery_consul
    ConsulSsl = walk_proplist(
                  ["cluster_formation", "peer_discovery_consul",
                   "ssl_options"],
                  proplists:get_value(rabbit, Config, [])),
    ?assertEqual(verify_peer, proplists:get_value(verify, ConsulSsl)),
    ?assertEqual("/etc/rabbitmq/consul-ca.pem",
                 proplists:get_value(cacertfile, ConsulSsl)).

embedded_migration_provenance_is_visible_in_each_mapping_test() ->
    setup(),
    Schemas = [
        rabbit_validators_schema(),
        auth_backend_http_excerpt()
    ],
    {_T, Mappings, _V} = cuttlefish_schema:strings(Schemas),
    Verify = find_mapping(
        ["auth_http", "ssl_options", "verify"], Mappings),
    [ProvLine | _] = cuttlefish_mapping:doc(Verify),
    ?assertEqual("(from partial sample_app:rabbit_ssl_options)",
                 ProvLine).

%% --- embedded migrated schema excerpts ---------------------------

%% The validators normally defined in rabbit.schema; the partial's
%% mappings reference them by name.
rabbit_validators_schema() ->
    "{validator, \"pem_file\", \"must be a readable PEM file\","
    "    fun(_) -> true end}.\n"
    "{validator, \"file_accessible\", \"must be a readable file\","
    "    fun(_) -> true end}.\n"
    "{validator, \"byte\", \"must be in 0..255\","
    "    fun(N) -> is_integer(N) andalso N >= 0 andalso N =< 255 end}.\n".

%% Mirrors the migrated rabbit.schema primary ssl_options block
%% (post-partial form).
rabbit_primary_ssl_excerpt() ->
    "{include_partial, {sample_app, \"rabbit_ssl_options\"},"
    " [{prefix, \"ssl_options\"},"
    "  {app_prefix, \"rabbit.ssl_options\"},"
    "  {disable_with, none}]}.\n".

%% Mirrors the migrated definitions.tls context.
rabbit_definitions_tls_excerpt() ->
    "{include_partial, {sample_app, \"rabbit_ssl_options\"},"
    " [{prefix, \"definitions.tls\"},"
    "  {app_prefix, \"rabbit.definitions.ssl_options\"}]}.\n"
    %% definitions.tls in the real schema overrides the partial's
    %% binary password translation with a string one.
    "{translation, \"rabbit.definitions.ssl_options.password\","
    "    fun(Conf) -> rabbit_cuttlefish:optionally_tagged_string("
    "                    \"definitions.tls.password\", Conf) end}.\n".

%% Mirrors the migrated rabbitmq_auth_backend_http schema's TLS block.
auth_backend_http_excerpt() ->
    "{include_partial, {sample_app, \"rabbit_ssl_options\"},"
    " [{prefix, \"auth_http.ssl_options\"},"
    "  {app_prefix, \"rabbitmq_auth_backend_http.ssl_options\"},"
    "  {disable_with, none}]}.\n".

%% Mirrors the migrated rabbitmq_auth_backend_ldap schema's TLS block.
auth_backend_ldap_excerpt() ->
    "{include_partial, {sample_app, \"rabbit_ssl_options\"},"
    " [{prefix, \"auth_ldap.ssl_options\"},"
    "  {app_prefix, \"rabbitmq_auth_backend_ldap.ssl_options\"},"
    "  {disable_with, none}]}.\n".

%% Mirrors the migrated rabbitmq_trust_store schema's TLS block.
trust_store_excerpt() ->
    "{include_partial, {sample_app, \"rabbit_ssl_options\"},"
    " [{prefix, \"trust_store.ssl_options\"},"
    "  {app_prefix, \"rabbitmq_trust_store.ssl_options\"},"
    "  {disable_with, none}]}.\n".

%% Mirrors the migrated rabbitmq_peer_discovery_consul schema's TLS block.
peer_discovery_consul_excerpt() ->
    "{include_partial, {sample_app, \"rabbit_ssl_options\"},"
    " [{prefix, \"cluster_formation.consul.ssl_options\"},"
    "  {app_prefix,"
    "   \"rabbit.cluster_formation.peer_discovery_consul.ssl_options\"},"
    "  {disable_with, none}]}.\n".

%% --- helpers ------------------------------------------------------

setup() ->
    add_fixture_path(),
    ensure_rabbit_cuttlefish_loaded().

add_fixture_path() ->
    Ebin = fixture_ebin(),
    ok = ensure_sample_app_file(Ebin),
    true = filelib:is_dir(Ebin) orelse
        erlang:error({fixture_ebin_missing, Ebin}),
    code:add_pathz(Ebin),
    ok.

%% Write `sample_app.app` if absent; rebar3 .gitignore excludes
%% `ebin/`, so the fixture .app file isn't carried in version control.
ensure_sample_app_file(Ebin) ->
    ok = filelib:ensure_dir(filename:join(Ebin, "marker")),
    AppFile = filename:join(Ebin, "sample_app.app"),
    case filelib:is_regular(AppFile) of
        true  -> ok;
        false ->
            App = "{application, sample_app,\n"
                  " [{description, \"Test fixture for cuttlefish partials\"},\n"
                  "  {vsn, \"0.0.1\"},\n"
                  "  {modules, []},\n"
                  "  {registered, []},\n"
                  "  {applications, [kernel, stdlib]},\n"
                  "  {env, []}]}.\n",
            ok = file:write_file(AppFile, App)
    end.

fixture_ebin() ->
    %% Resolve from cuttlefish's lib_dir (always set during tests) up
    %% 4 levels to the project root, then into the source-tree fixture.
    %% Avoids depending on rebar3 copying test/fixtures into _build.
    LibDir = code:lib_dir(cuttlefish),
    Project = filename:absname(
                filename:join([LibDir, "..", "..", "..", ".."])),
    filename:join([Project, "test", "fixtures",
                   "sample_app", "ebin"]).

%% Compile + load a `rabbit_cuttlefish` stub purely in memory so
%% rebar3 doesn't try to treat it as a project source. The partial's
%% password translation calls this module; without it the test would
%% crash when conf sets `<prefix>.password'.
ensure_rabbit_cuttlefish_loaded() ->
    case code:is_loaded(rabbit_cuttlefish) of
        false ->
            Forms = stub_forms(),
            {ok, rabbit_cuttlefish, Beam} =
                compile:forms(Forms, [return_errors]),
            {module, rabbit_cuttlefish} =
                code:load_binary(rabbit_cuttlefish, "<inline-stub>", Beam),
            ok;
        _ -> ok
    end.

stub_forms() ->
    Source =
        "-module(rabbit_cuttlefish).\n"
        "-export([optionally_tagged_binary/2, optionally_tagged_string/2]).\n"
        "optionally_tagged_binary(Key, Conf) ->\n"
        "    case cuttlefish:conf_get(Key, Conf, undefined) of\n"
        "        undefined            -> cuttlefish:unset();\n"
        "        Bin when is_binary(Bin) -> Bin;\n"
        "        Str when is_list(Str) -> list_to_binary(Str);\n"
        "        {_, V} when is_binary(V) -> {encrypted, V};\n"
        "        {_, V} when is_list(V) -> {encrypted, list_to_binary(V)}\n"
        "    end.\n"
        "optionally_tagged_string(Key, Conf) ->\n"
        "    case cuttlefish:conf_get(Key, Conf, undefined) of\n"
        "        undefined -> cuttlefish:unset();\n"
        "        V -> V\n"
        "    end.\n",
    {ok, Tokens, _} = erl_scan:string(Source),
    [parse_form(F) || F <- split_dots(Tokens, [], [])].

split_dots([], [], Acc) -> lists:reverse(Acc);
split_dots([{dot, _} = D | Rest], Cur, Acc) ->
    split_dots(Rest, [], [lists:reverse([D | Cur]) | Acc]);
split_dots([T | Rest], Cur, Acc) ->
    split_dots(Rest, [T | Cur], Acc).

parse_form(Tokens) ->
    {ok, Form} = erl_parse:parse_form(Tokens),
    Form.

include_for(Prefix, AppPrefix) ->
    lists:flatten(io_lib:format(
        "{include_partial, {sample_app, \"rabbit_ssl_options\"},"
        " [{prefix, ~tp}, {app_prefix, ~tp}]}.\n",
        [Prefix, AppPrefix])).

%% Compose into a single schema string so the consumer's inline
%% override appears AFTER the include in the same parse stream and
%% the standard "later wins" semantics apply.
compose(Tail) when is_list(Tail) ->
    [rabbit_validators_schema() ++ Tail].
compose(A, B) ->
    [rabbit_validators_schema() ++ A ++ B].
compose(A, B, C, D) ->
    [rabbit_validators_schema() ++ A ++ B ++ C ++ D].

lookup(App, Key, Config) ->
    proplists:get_value(Key, proplists:get_value(App, Config, []), []).

walk_proplist([], V) -> V;
walk_proplist([Seg | Rest], Cfg) ->
    walk_proplist(Rest, proplists:get_value(list_to_atom(Seg), Cfg, [])).

find_mapping(Var, Mappings) ->
    case [M || M <- Mappings, cuttlefish_mapping:variable(M) =:= Var] of
        [M] -> M;
        [] -> erlang:error({mapping_not_found, Var})
    end.
