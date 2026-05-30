-module(cuttlefish_bytesize_duration_constraints_tests).

-include_lib("eunit/include/eunit.hrl").

-import(cuttlefish_datatypes, [from_string/2, to_string/2, is_supported/1]).

%% bytesize range constraints

bytesize_with_constraints_is_supported_test() ->
    ?assert(is_supported({bytesize, [{min, 0}, {max, 1024}]})),
    ?assert(is_supported({bytesize, non_negative})),
    ?assert(is_supported({bytesize, positive})).

bytesize_rejects_invalid_constraints_test() ->
    ?assertNot(is_supported({bytesize, [{min, "zero"}]})),
    ?assertNot(is_supported({bytesize, [{nonsense, 1}]})).

bytesize_accepts_in_range_test() ->
    DT = {bytesize, [{min, 0}, {max, 1048576}]},
    ?assertEqual(1024, from_string("1KB", DT)),
    ?assertEqual(0, from_string("0", DT)).

bytesize_rejects_below_min_test() ->
    %% bytesize parses into an integer count of bytes; the bound
    %% applies to that count.
    DT = {bytesize, [{min, 1024}]},
    ?assertMatch({error, {range_violation, {512, {min, 1024}}}},
                 from_string("512", DT)).

bytesize_rejects_above_max_test() ->
    DT = {bytesize, [{max, 1024}]},
    ?assertMatch({error, {range_violation, {2048, {max, 1024}}}},
                 from_string("2KB", DT)).

bytesize_to_string_passes_through_test() ->
    DT = {bytesize, [{min, 0}]},
    ?assertEqual("1GB", to_string(1073741824, DT)).

%% duration range constraints

duration_with_constraints_is_supported_test() ->
    ?assert(is_supported({duration, ms, [{min, 0}, {max, 60000}]})),
    ?assert(is_supported({duration, s, non_negative})),
    ?assert(is_supported({duration, h, positive})).

duration_rejects_bad_unit_test() ->
    ?assertNot(is_supported({duration, year, [{min, 0}]})).

duration_accepts_in_range_test() ->
    DT = {duration, ms, [{min, 0}, {max, 60000}]},
    ?assertEqual(1000, from_string("1s", DT)),
    ?assertEqual(60000, from_string("1m", DT)).

duration_rejects_below_min_test() ->
    DT = {duration, ms, [{min, 1000}]},
    ?assertMatch({error, {range_violation, {500, {min, 1000}}}},
                 from_string("500ms", DT)).

duration_rejects_above_max_test() ->
    DT = {duration, s, [{max, 60}]},
    ?assertMatch({error, {range_violation, {120, {max, 60}}}},
                 from_string("2m", DT)).

duration_to_string_passes_through_test() ->
    %% to_string preserves the bare-duration semantics for the new
    %% three-tuple form.
    DT = {duration, s, [{min, 0}]},
    ?assertEqual("1w", to_string("1w", DT)).
