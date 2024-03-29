-module(cuttlefish_escript_integration_tests).

-include_lib("eunit/include/eunit.hrl").

escript_utf8_test() ->
    ExpectedSetting = "thingŒ",
    ExpectedSetting2 = "C:\\ProgramData\\RabbitMQ Sérvér\\Евгений\\rabbitmq.conf",
    ExpectedSetting3 = "ascii string",
    Expected = [{setting3, ExpectedSetting3},
                {setting2,ExpectedSetting2},
                {setting, ExpectedSetting}],

    ?debugFmt("ExpectedSetting ~tp~n", [ExpectedSetting]),
    ?debugFmt("ExpectedSetting2 ~tp~n", [ExpectedSetting2]),

    BaseDir = "test_fixtures/escript_utf8_test",
    EtcDir = filename:join(BaseDir, "etc"),
    LibDir = filename:join(BaseDir, "lib"),
    ConfFile = filename:join(EtcDir, "utf8.conf"),
    GeneratedConfigDir = filename:join(BaseDir, "generated.config"),

    _ = cuttlefish_test_logging:set_up(),
    _ = cuttlefish_test_logging:bounce(error),

    %% Empty workspace
    case file:list_dir(GeneratedConfigDir) of
        {ok, FilenamesToDelete} ->
            [ file:delete(filename:join([GeneratedConfigDir, F])) || F <- FilenamesToDelete ];
        _ -> ok
    end,

    Args = io_lib:format("-d ~ts -s ~ts -e ~ts -c ~ts generate",
                         [GeneratedConfigDir, LibDir, EtcDir, ConfFile]),
    ok = cuttlefish_escript:main(Args),
    [] = cuttlefish_test_logging:get_logs(),

    [AppConfig0] = filelib:wildcard("app.*.config", GeneratedConfigDir),
    AppConfig1 = filename:join(GeneratedConfigDir, AppConfig0),
    {ok, [Actual]} = file:consult(AppConfig1),
    ?assertMatch(Expected, Actual),
    ok.

advanced_config_format_test() ->
    _ = cuttlefish_test_logging:set_up(),
    _ = cuttlefish_test_logging:bounce(error),
    ?assertThrow(stop_deactivate, cuttlefish_escript:main(
                                    "-d test_fixtures/acformat/generated.config "
                                    "-s test_fixtures/acformat/lib "
                                    "-e test_fixtures/acformat/etc "
                                    "-c test_fixtures/acformat/etc/acformat.conf generate"
                                   )),
    [Log] = cuttlefish_test_logging:get_logs(),
    ?assertMatch({match, _}, re:run(Log, "Error parsing test_fixtures/acformat/etc/advanced.config, incorrect format: \\[\\[a\\],\\[b\\]\\]")),
    ok.

escript_prune_test_() ->
    {timeout, 20, [
                   escript_prune("-m 3", 3),
                   escript_prune("", 3), %% default
                   escript_prune("-m 6", 6)
                  ]}.

escript_prune(DashM, ExpectedMax) ->
    %% Empty workspace
    case file:list_dir("test_fixtures/escript_prune_test/generated.config") of
        {ok, FilenamesToDelete} ->
            [ file:delete(filename:join(["test_fixtures/escript_prune_test/generated.config",F])) || F <- FilenamesToDelete ];
        _ -> ok
    end,

    {_, _, T} = lists:foldl(
        fun(Counter, {PrevConfigs, PrevVMArgs, Tests}) ->
            io:format("Running iteration: ~tp", [Counter]),
            %% Timer to keep from generating more than one file per second
            timer:sleep(1100),
            cuttlefish_escript:main(
              "-d test_fixtures/escript_prune_test/generated.config "
              "-s test_fixtures/escript_prune_test/lib "
              "-e test_fixtures/escript_prune_test/etc "
              ++ DashM ++ " generate"
            ),

            AppConfigs =
                lists:sort(
                    filelib:wildcard("app.*.config",
                                     "test_fixtures/escript_prune_test/generated.config")),
            VMArgs =
                lists:sort(
                    filelib:wildcard("vm.*.args",
                                     "test_fixtures/escript_prune_test/generated.config")),

            {AppConfigs,
             VMArgs,

             [?_assert(length(AppConfigs) =< ExpectedMax),
              ?_assert(length(VMArgs) =< ExpectedMax),
              compare_lists(PrevConfigs, AppConfigs),
              compare_lists(PrevVMArgs, VMArgs) | Tests]}
        end,
        {[], [], []},
        lists:seq(1,10)),
    T.

%% This function is asserting that Previous is the tail of Current OR
%% that the tail of Previous is equal to the first length(Previous)
%% elements of Current
compare_lists(Previous, Current) when (length(Previous) +1) =:= length(Current) ->
    compare_lists([stub|Previous], Current);
compare_lists([_|PTail] = Previous, Current) when length(Previous) =:= length(Current) ->
    NewPrevious = PTail ++ [lists:last(Current)],
    ?_assertEqual(NewPrevious, Current);
compare_lists(_Previous, _Current) ->
    ?_assert(false).
