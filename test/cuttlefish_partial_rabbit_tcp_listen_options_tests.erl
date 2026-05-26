-module(cuttlefish_partial_rabbit_tcp_listen_options_tests).

-include_lib("eunit/include/eunit.hrl").

single_include_with_realistic_conf_test() ->
    add_fixture_path(),
    Schema =
        "{include_partial, {sample_app, \"rabbit_tcp_listen_options\"},\n"
        "    [{prefix, \"mqtt.tcp_listen_options\"},\n"
        "     {app_prefix, \"rabbitmq_mqtt.tcp_listen_options\"},\n"
        "     {disable_with, none}]}.\n",
    SchemaTuple = cuttlefish_schema:strings([Schema]),
    Conf = [{["mqtt", "tcp_listen_options", "backlog"],     128},
            {["mqtt", "tcp_listen_options", "nodelay"],     true},
            {["mqtt", "tcp_listen_options", "port"],        1883},
            {["mqtt", "tcp_listen_options", "linger", "on"],      true},
            {["mqtt", "tcp_listen_options", "linger", "timeout"], 10}],
    Config = cuttlefish_generator:map(SchemaTuple, Conf),
    TcpOpts = lookup(rabbitmq_mqtt, tcp_listen_options, Config),
    ?assertEqual(128,   proplists:get_value(backlog, TcpOpts)),
    ?assertEqual(true,  proplists:get_value(nodelay, TcpOpts)),
    ?assertEqual(1883,  proplists:get_value(port, TcpOpts)),
    ?assertEqual({true, 10}, proplists:get_value(linger, TcpOpts)).

linger_aggregation_uses_defaults_when_only_one_set_test() ->
    add_fixture_path(),
    Schema =
        "{include_partial, {sample_app, \"rabbit_tcp_listen_options\"},\n"
        "    [{prefix, \"x.tcp\"},\n"
        "     {app_prefix, \"y.tcp\"},\n"
        "     {disable_with, none}]}.\n",
    SchemaTuple = cuttlefish_schema:strings([Schema]),
    Conf = [{["x", "tcp", "linger", "timeout"], 5}],
    Config = cuttlefish_generator:map(SchemaTuple, Conf),
    Linger = proplists:get_value(linger, lookup(y, tcp, Config)),
    ?assertEqual({false, 5}, Linger).

port_validator_rejects_out_of_range_value_test() ->
    add_fixture_path(),
    Schema =
        "{include_partial, {sample_app, \"rabbit_tcp_listen_options\"},\n"
        "    [{prefix, \"x.tcp\"},\n"
        "     {app_prefix, \"y.tcp\"},\n"
        "     {disable_with, none}]}.\n",
    SchemaTuple = cuttlefish_schema:strings([Schema]),
    Conf = [{["x", "tcp", "port"], 70000}],
    Result = cuttlefish_generator:map(SchemaTuple, Conf),
    ?assertMatch({error, validation, _}, Result).

disable_with_short_circuit_at_parent_test() ->
    add_fixture_path(),
    Schema =
        "{include_partial, {sample_app, \"rabbit_tcp_listen_options\"},\n"
        "    [{prefix, \"x.tcp\"},\n"
        "     {app_prefix, \"y.tcp\"},\n"
        "     {disable_with, none}]}.\n",
    SchemaTuple = cuttlefish_schema:strings([Schema]),
    Conf = [{["x", "tcp"], none}],
    Config = cuttlefish_generator:map(SchemaTuple, Conf),
    ?assertEqual([], proplists:get_value(tcp, proplists:get_value(y, Config))).

four_listener_contexts_in_one_schema_test() ->
    add_fixture_path(),
    Schema = lists:flatten([
        include_for("rabbit.tcp_listen_options", "rabbit.tcp_listen_options"),
        include_for("mqtt.tcp_listen_options", "rabbitmq_mqtt.tcp_listen_options"),
        include_for("stomp.tcp_listen_options", "rabbitmq_stomp.tcp_listen_options"),
        include_for("stream.tcp_listen_options", "rabbitmq_stream.tcp_listen_options")
    ]),
    SchemaTuple = cuttlefish_schema:strings([Schema]),
    Conf = [{["rabbit", "tcp_listen_options", "port"], 5672},
            {["mqtt",   "tcp_listen_options", "port"], 1883},
            {["stomp",  "tcp_listen_options", "port"], 61613},
            {["stream", "tcp_listen_options", "port"], 5552}],
    Config = cuttlefish_generator:map(SchemaTuple, Conf),
    ?assertEqual(5672,
                 proplists:get_value(port,
                                     lookup(rabbit, tcp_listen_options, Config))),
    ?assertEqual(1883,
                 proplists:get_value(port,
                                     lookup(rabbitmq_mqtt, tcp_listen_options, Config))),
    ?assertEqual(61613,
                 proplists:get_value(port,
                                     lookup(rabbitmq_stomp, tcp_listen_options, Config))),
    ?assertEqual(5552,
                 proplists:get_value(port,
                                     lookup(rabbitmq_stream, tcp_listen_options, Config))).

%% --- helpers ------------------------------------------------------

include_for(Prefix, AppPrefix) ->
    lists:flatten(io_lib:format(
        "{include_partial, {sample_app, \"rabbit_tcp_listen_options\"},\n"
        "    [{prefix, ~tp},\n"
        "     {app_prefix, ~tp},\n"
        "     {disable_with, none}]}.\n",
        [Prefix, AppPrefix])).

lookup(App, Key, Config) ->
    proplists:get_value(Key, proplists:get_value(App, Config, []), []).

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
