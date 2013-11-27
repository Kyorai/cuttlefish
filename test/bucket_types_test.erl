-module(bucket_types_test).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

bucket_types_prototype_test() ->
    RawConf = lists:flatten([
            "n_val = 2\n",
            "r = 4\n",
            "siblings = on\n"
        ]),

    Conf = conf_parse:parse(RawConf),
    Schema = cuttlefish_schema:file("../test/bucket_type.schema"),
    BucketConfig = cuttlefish_generator:map(Schema, Conf),

    %%io:format("~p~n", [BucketConfig]),
    ?assertEqual([{n_val,2},{r,4},{allow_mult,true}], BucketConfig),
    ok.
