-module(cuttlefish_subs_integration_tests).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

basic_rhs_subs_1_test() ->
  Schema = cuttlefish_schema:file("test/example1.schema"),
  Conf = [
    {["example1", "ab"], "ab-ab"},
    {["example1", "cd"], "$(example1.ab)"}
  ],
  Generated = cuttlefish_generator:map(Schema, Conf),
  ?assertEqual([
    {example1, [
      {cd, "ab-ab"},
      {ab, "ab-ab"}
    ]}
  ], Generated),
  ok.


breaks_on_rhs_not_found_test() ->
  Schema = cuttlefish_schema:file("test/riak.schema"),
  Conf = [{["ring", "state_dir"], "$(tyktorp)/ring"}],
  ?assertMatch({error, rhs_subs, _}, cuttlefish_generator:map(Schema, Conf)),
  ok.

breaks_on_rhs_infinite_loop_test() ->
  Schema = cuttlefish_schema:file("test/riak.schema"),
  Conf = [
          {["ring", "state_dir"], "$(platform_data_dir)/ring"},
          {["platform_data_dir"], "$(ring.state_dir)/data"}
         ],
  ?assertMatch({error, rhs_subs, _}, cuttlefish_generator:map(Schema, Conf)),
  ok.