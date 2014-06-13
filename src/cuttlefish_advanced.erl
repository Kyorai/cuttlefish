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
    orddict:merge(fun(Key,Proplist,Overlay) -> 
                      overlay_property(Key, Overlay, Proplist) 
                  end, 
                  orddict:from_list(GeneratedConfig), orddict:from_list(AdvancedConfig)).

overlay_property(_, [E] = Value, Proplist) when is_list(Value) andalso is_tuple(E) -> 
    {SearchKey, SearchVal} = E,    
    case is_list(SearchVal) andalso is_tuple(hd(SearchVal)) of 
        true ->
            case proplists:get_value(SearchKey, Proplist) of 
                undefined ->
                    Proplist;
                NextProplist ->
                    R = overlay_property(SearchKey, SearchVal, NextProplist),
                    [{SearchKey, R}]
            end;
	false ->
	    lists:keystore(SearchKey, 1, Proplist, E)
    end;
overlay_property(Key, Value, []) -> 
    [{Key, Value}];
overlay_property(Key, Value, Proplist) -> 
    lists:keystore(Key, 1, Value, Proplist).
   
-ifdef(TEST).

overlay_test() ->
    GeneratedConfig = [
        {app1, [{'setting1.1', "value1.1"}]},
        {app2, [{'setting2.1', "value2.1"}]},
        {app3, [{'setting3.1', [{"blah", "blah"}, {"blarg", "blarg"}]}]},
        {app6, [{'setting6.1', [{'setting6.2', [{"manray", "monkey"}, {"picadillo", "porkpie"}]}]}]},
        {app7, [{'setting7.1', [{'setting7.2', [{"manray", "monkey"}, {"picadillo", "porkpie"}]}]}]}

    ],

    AdvancedConfig = [
        {app3, [{'setting3.1', [{"blarg", "blorg"}]}]},
        {app4, [{'setting4.1', i_dont_care}]},
        {app5, [{'some_unschemad_thing', 'like_a_penguin'}]},
        {app6, [{'setting6.1', [{'setting6.2', [{"manray", "moonstone"}]}]}]},
        {app7, [{'setting7.1', [{'setting7.2', [{"picadillo", "pizzapie"}]}]}]}

    ],

    Expected = [
        {app1, [{'setting1.1', "value1.1"}]},
        {app2, [{'setting2.1', "value2.1"}]},
        {app3, [{'setting3.1',  [{"blah", "blah"}, {"blarg", "blorg"}]}]},
        {app4, [{'setting4.1', i_dont_care}]},
        {app5, [{'some_unschemad_thing', 'like_a_penguin'}]},
        {app6, [{'setting6.1', [{'setting6.2', [{"manray", "moonstone"}, {"picadillo", "porkpie"}]}]}]},
        {app7, [{'setting7.1', [{'setting7.2', [{"manray", "monkey"}, {"picadillo", "pizzapie"}]}]}]}

    ],

    NewConfig = overlay(GeneratedConfig, AdvancedConfig),
    %?debugFmt("NewConfig:~p, Expected:~p", [NewConfig, Expected]),
    ?assertEqual(Expected, NewConfig),

    ok.

-endif.
