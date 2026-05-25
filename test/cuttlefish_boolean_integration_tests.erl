-module(cuttlefish_boolean_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SCHEMA, "{mapping, \"enable.thing\", \"myapp.enable_thing\", "
                "[{datatype, boolean}, {default, true}]}.").

default_is_applied_test() ->
    Schema = cuttlefish_schema:strings([?SCHEMA]),
    Conf = conf_parse:parse(<<>>),
    Config = cuttlefish_generator:map(Schema, Conf),
    ?assertEqual(true, lookup(Config, myapp, enable_thing)).

override_with_false_test() ->
    Schema = cuttlefish_schema:strings([?SCHEMA]),
    Conf = conf_parse:parse(<<"enable.thing = false\n">>),
    Config = cuttlefish_generator:map(Schema, Conf),
    ?assertEqual(false, lookup(Config, myapp, enable_thing)).

override_with_true_test() ->
    Schema = cuttlefish_schema:strings([?SCHEMA]),
    Conf = conf_parse:parse(<<"enable.thing = true\n">>),
    Config = cuttlefish_generator:map(Schema, Conf),
    ?assertEqual(true, lookup(Config, myapp, enable_thing)).

%% Bad input bubbles out as a pipeline error rather than as a config value.
invalid_value_fails_pipeline_test() ->
    Schema = cuttlefish_schema:strings([?SCHEMA]),
    Conf = conf_parse:parse(<<"enable.thing = yes\n">>),
    ?assertMatch({error, transform_datatypes, _},
                 cuttlefish_generator:map(Schema, Conf)).

%% Translations run after the datatype pipeline, so a `boolean` mapping's
%% translation sees the value as an Erlang `true`/`false` atom, not a string.
translation_sees_typed_value_test() ->
    Schema = cuttlefish_schema:strings([
        "{mapping, \"enable.thing\", \"myapp.enable_thing\", "
            "[{datatype, boolean}, {default, false}]}.",
        "{translation, \"myapp.enable_thing\", "
            "fun(Conf) -> not cuttlefish:conf_get(\"enable.thing\", Conf) end}."
    ]),
    Conf = conf_parse:parse(<<"enable.thing = true\n">>),
    Config = cuttlefish_generator:map(Schema, Conf),
    ?assertEqual(false, lookup(Config, myapp, enable_thing)).

lookup(Config, App, Key) ->
    proplists:get_value(Key, proplists:get_value(App, Config)).
