-module(cuttlefish_partial_rabbit_combined_tests).

-include_lib("eunit/include/eunit.hrl").

mqtt_shaped_plugin_schema_combines_both_partials_test() ->
    add_fixture_path(),
    Schema =
        validators_schema() ++
        "{include_partial, {sample_app, \"rabbit_ssl_options\"},\n"
        "    [{prefix, \"mqtt.ssl_options\"},\n"
        "     {app_prefix, \"pseudo_mqtt.ssl_options\"}]}.\n"
        "{include_partial, {sample_app, \"rabbit_tcp_listen_options\"},\n"
        "    [{prefix, \"mqtt.tcp_listen_options\"},\n"
        "     {app_prefix, \"pseudo_mqtt.tcp_listen_options\"},\n"
        "     {disable_with, none}]}.\n"
        "{mapping, \"mqtt.protocol_version\", \"pseudo_mqtt.protocol_version\",\n"
        "    [{datatype, {enum, ['3.1.1', '5.0']}},\n"
        "     {default, '5.0'}]}.\n",
    SchemaTuple = cuttlefish_schema:strings([Schema]),
    Conf = [
        {["mqtt", "ssl_options", "verify"],      verify_peer},
        {["mqtt", "ssl_options", "cacertfile"], "/etc/ca.pem"},
        {["mqtt", "ssl_options", "versions", "tlsv1.3"], 'tlsv1.3'},
        {["mqtt", "tcp_listen_options", "port"],    1883},
        {["mqtt", "tcp_listen_options", "backlog"], 256},
        {["mqtt", "protocol_version"], '5.0'}
    ],
    Config = cuttlefish_generator:map(SchemaTuple, Conf),
    PseudoMqtt = proplists:get_value(pseudo_mqtt, Config),
    SslOpts = proplists:get_value(ssl_options, PseudoMqtt),
    TcpOpts = proplists:get_value(tcp_listen_options, PseudoMqtt),

    ?assertEqual(verify_peer,  proplists:get_value(verify, SslOpts)),
    ?assertEqual("/etc/ca.pem", proplists:get_value(cacertfile, SslOpts)),
    ?assertEqual(['tlsv1.3'],   proplists:get_value(versions, SslOpts)),
    ?assertEqual(1883,          proplists:get_value(port, TcpOpts)),
    ?assertEqual(256,           proplists:get_value(backlog, TcpOpts)),
    ?assertEqual('5.0',         proplists:get_value(protocol_version, PseudoMqtt)).

provenance_is_attached_to_both_partials_mappings_test() ->
    add_fixture_path(),
    Schema =
        validators_schema() ++
        "{include_partial, {sample_app, \"rabbit_ssl_options\"},\n"
        "    [{prefix, \"mqtt.ssl_options\"},\n"
        "     {app_prefix, \"pseudo_mqtt.ssl_options\"}]}.\n"
        "{include_partial, {sample_app, \"rabbit_tcp_listen_options\"},\n"
        "    [{prefix, \"mqtt.tcp_listen_options\"},\n"
        "     {app_prefix, \"pseudo_mqtt.tcp_listen_options\"},\n"
        "     {disable_with, none}]}.\n",
    {_T, Mappings, _V} = cuttlefish_schema:strings([Schema]),
    Verify = find_mapping(["mqtt", "ssl_options", "verify"], Mappings),
    Port = find_mapping(["mqtt", "tcp_listen_options", "port"], Mappings),
    [VerifyProv | _] = cuttlefish_mapping:doc(Verify),
    [PortProv | _]   = cuttlefish_mapping:doc(Port),
    ?assertEqual("(from partial sample_app:rabbit_ssl_options)", VerifyProv),
    ?assertEqual("(from partial sample_app:rabbit_tcp_listen_options)", PortProv).

%% --- helpers ------------------------------------------------------

find_mapping(Var, Mappings) ->
    case [M || M <- Mappings, cuttlefish_mapping:variable(M) =:= Var] of
        [M] -> M;
        [] -> erlang:error({mapping_not_found, Var})
    end.

validators_schema() ->
    "{validator, \"pem_file\", \"must be a PEM file\","
    "    fun(_) -> true end}.\n"
    "{validator, \"file_accessible\", \"must be a readable file\","
    "    fun(_) -> true end}.\n"
    "{validator, \"byte\", \"must be in 0..255\","
    "    fun(N) -> is_integer(N) andalso N >= 0 andalso N =< 255 end}.\n"
    "{validator, \"port\", \"must be in 0..65535\","
    "    fun(N) -> is_integer(N) andalso N >= 0 andalso N =< 65535 end}.\n"
    "{validator, \"non_negative_integer\", \"must be >= 0\","
    "    fun(N) -> is_integer(N) andalso N >= 0 end}.\n".

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
