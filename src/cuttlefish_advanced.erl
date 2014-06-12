%% -------------------------------------------------------------------
%%
%% cuttlefish_advanced: handles merging of advanced configs
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

-module(cuttlefish_advanced).

-export([overlay/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

%% @doc this function overlays the values in proplist 'AdvancedConfig'
%% on top of 'GeneratedConfig'
overlay(GeneratedConfig, AdvancedConfig) ->
    lists:foldl(
        fun({ApplicationName, ApplicationConfig}, OuterAcc) ->
            GeneratedApplicationConfig = proplists:get_value(ApplicationName, GeneratedConfig, []),
            Updated = lists:foldl(
                fun({ConfigElementName, ConfigElement}, Acc) ->
                    overlay_property(ConfigElementName, ConfigElement, Acc)

                end,
                GeneratedApplicationConfig,
                ApplicationConfig),
            overlay_property(ApplicationName, Updated, OuterAcc)
        end,
        GeneratedConfig,
        AdvancedConfig).

%% @doc overlay a property
-spec overlay_property(atom() | string(), any(), [{string(), any()}]) -> [{string(), any()}].
overlay_property(Key, [E] = Value, Proplist) when is_list(Value) andalso is_tuple(E) -> 
    case lists:keyfind(Key, 1, Proplist) of 
        false ->
            lists:keystore(Key, 1, Proplist, {Key, Value});
        {_, SubPropvals} ->
            {ReplaceKey, _ReplaceVal} = E,
            NewSubValue = lists:keyreplace(ReplaceKey, 1, SubPropvals, E),
            NewConfig = {Key, NewSubValue},
	    lists:keyreplace(Key, 1, Proplist, NewConfig)
    end;
overlay_property(Key, Value, Proplist) ->
    lists:keystore(Key, 1, Proplist, {Key, Value}).

-ifdef(TEST).

overlay_test() ->
    GeneratedConfig = [
        {app1, [{'setting1.1', "value1.1"}]},
        {app2, [{'setting2.1', "value2.1"}]},
        {app3, [{'setting3.1', [{"blah", "blah"}, {"blarg", "blarg"}]}]},
%        {app6, [{'setting6.1', [{'setting6.2', [{"manray", "monkey"}, {"picadillo", "porkpie"}]}]}]}
        {app6, [{'setting6.1', [{'setting6.2', [{"manray", "monkey"}]}]}]}

    ],

    AdvancedConfig = [
        {app3, [{'setting3.1', [{"blarg", "blorg"}]}]},
        {app4, [{'setting4.1', i_dont_care}]},
        {app5, [{'some_unschemad_thing', 'like_a_penguin'}]},
        {app6, [{'setting6.1', [{'setting6.2', [{"manray", "moonstone"}]}]}]}
    ],

    Expected = [
        {app1, [{'setting1.1', "value1.1"}]},
        {app2, [{'setting2.1', "value2.1"}]},
        {app3, [{'setting3.1',  [{"blah", "blah"}, {"blarg", "blorg"}]}]},
        {app6, [{'setting6.1', [{'setting6.2', [{"manray", "moonstone"}]}]}]},
%        {app6, [{'setting6.1', [{'setting6.2', [{"manray", "moonstone"}, {"picadillo", "porkpie"}]}]}]}
        {app4, [{'setting4.1', i_dont_care}]},
        {app5, [{'some_unschemad_thing', 'like_a_penguin'}]}
    ],

    NewConfig = overlay(GeneratedConfig, AdvancedConfig),
    ?assertEqual(Expected, NewConfig),

    ok.

-endif.
