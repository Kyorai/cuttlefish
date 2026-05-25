%% -------------------------------------------------------------------
%%
%% cuttlefish_paths: filesystem path helpers
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(cuttlefish_paths).

-export([tmp_base/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec tmp_base() -> file:filename_all().
tmp_base() ->
    Candidates = [os:getenv("TMPDIR"),
                  os:getenv("TEMP"),
                  os:getenv("TMP")],
    case [D || D <- Candidates, is_list(D), D =/= ""] of
        [Dir | _] -> Dir;
        []        -> "."
    end.

-ifdef(TEST).

tmp_base_prefers_tmpdir_over_temp_and_tmp_test() ->
    with_env([{"TMPDIR", "/from-tmpdir"},
              {"TEMP",   "/from-temp"},
              {"TMP",    "/from-tmp"}],
             fun() -> ?assertEqual("/from-tmpdir", tmp_base()) end).

tmp_base_falls_through_to_temp_then_tmp_test() ->
    with_env([{"TMPDIR", false},
              {"TEMP",   "/from-temp"},
              {"TMP",    "/from-tmp"}],
             fun() -> ?assertEqual("/from-temp", tmp_base()) end),
    with_env([{"TMPDIR", false},
              {"TEMP",   false},
              {"TMP",    "/from-tmp"}],
             fun() -> ?assertEqual("/from-tmp", tmp_base()) end).

tmp_base_ignores_empty_env_vars_test() ->
    with_env([{"TMPDIR", ""},
              {"TEMP",   ""},
              {"TMP",    "/real"}],
             fun() -> ?assertEqual("/real", tmp_base()) end).

tmp_base_falls_back_to_cwd_when_unset_test() ->
    with_env([{"TMPDIR", false},
              {"TEMP",   false},
              {"TMP",    false}],
             fun() -> ?assertEqual(".", tmp_base()) end).

%% Snapshot the requested env vars, apply overrides, run `Body',
%% then restore. `false' as a value means "unset for the duration".
with_env(Overrides, Body) ->
    Saved = [{Var, os:getenv(Var)} || {Var, _} <- Overrides],
    try
        lists:foreach(fun apply_env/1, Overrides),
        Body()
    after
        lists:foreach(fun apply_env/1, Saved)
    end.

apply_env({Var, false}) -> os:unsetenv(Var);
apply_env({Var, Val})   -> os:putenv(Var, Val).

-endif.
