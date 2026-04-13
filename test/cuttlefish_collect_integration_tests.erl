%% -------------------------------------------------------------------
%%
%% Integration tests for the {collect, Type} mapping property.
%%
%% Each test pair demonstrates equivalence between the current RabbitMQ
%% schema style (explicit translation) and the new {collect, Type} form.
%% Schema patterns are taken verbatim from
%% deps/rabbit/priv/schema/rabbit.schema in the rabbitmq-server repository.
%%
%% Copyright (c) 2024-2026 Broadcom Inc. or its subsidiaries. All Rights Reserved.
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
-module(cuttlefish_collect_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%%
%% Pattern 1: ssl_options.versions.$version → rabbit.ssl_options.versions
%%
%% From rabbit.schema lines 445-452. Collects all configured TLS version
%% atoms into a plain list. The {collect, list} form is a drop-in replacement
%% for the translation.
%%

%% Current form — explicit translation, exactly as in rabbit.schema.
ssl_versions_current_form_test() ->
    Mappings = [
        cuttlefish_mapping:parse({mapping, "ssl_options.versions.$version",
                                  "rabbit.ssl_options.versions",
                                  [{datatype, atom}]})
    ],
    Translations = [
        cuttlefish_translation:parse(
            {translation, "rabbit.ssl_options.versions",
             fun(Conf) ->
                 Settings = cuttlefish_variable:filter_by_prefix(
                              "ssl_options.versions", Conf),
                 [V || {_, V} <- Settings]
             end})
    ],
    Conf = [
        {["ssl_options", "versions", "tlsv1.3"], 'tlsv1.3'},
        {["ssl_options", "versions", "tlsv1.2"], 'tlsv1.2'}
    ],
    Result = cuttlefish_generator:map({Translations, Mappings, []}, Conf),
    ?assertMatch([{rabbit, [{ssl_options, [{versions, _}]}]}], Result),
    [{rabbit, [{ssl_options, [{versions, Versions}]}]}] = Result,
    ?assertEqual(['tlsv1.2', 'tlsv1.3'], lists:sort(Versions)),
    ok.

%% New form — {collect, list} replaces the translation entirely.
ssl_versions_collect_form_test() ->
    Mappings = [
        cuttlefish_mapping:parse({mapping, "ssl_options.versions.$version",
                                  "rabbit.ssl_options.versions",
                                  [{datatype, atom}, {collect, list}]})
    ],
    Conf = [
        {["ssl_options", "versions", "tlsv1.3"], 'tlsv1.3'},
        {["ssl_options", "versions", "tlsv1.2"], 'tlsv1.2'}
    ],
    Result = cuttlefish_generator:map({[], Mappings, []}, Conf),
    ?assertMatch([{rabbit, [{ssl_options, [{versions, _}]}]}], Result),
    [{rabbit, [{ssl_options, [{versions, Versions}]}]}] = Result,
    %% {collect, list} sorts by the $version segment ("tlsv1.2" < "tlsv1.3").
    ?assertEqual(['tlsv1.2', 'tlsv1.3'], Versions),
    ok.

%% Absent conf → key must not appear in app.config (both forms agree).
ssl_versions_absent_current_form_test() ->
    Mappings = [
        cuttlefish_mapping:parse({mapping, "ssl_options.versions.$version",
                                  "rabbit.ssl_options.versions",
                                  [{datatype, atom}]})
    ],
    Translations = [
        cuttlefish_translation:parse(
            {translation, "rabbit.ssl_options.versions",
             fun(Conf) ->
                 Settings = cuttlefish_variable:filter_by_prefix(
                              "ssl_options.versions", Conf),
                 [V || {_, V} <- Settings]
             end})
    ],
    Conf = [],
    Result = cuttlefish_generator:map({Translations, Mappings, []}, Conf),
    %% Translation returns [] which cuttlefish treats as a valid empty list.
    rabbit_ssl_versions_absent_assert(Result).

ssl_versions_absent_collect_form_test() ->
    Mappings = [
        cuttlefish_mapping:parse({mapping, "ssl_options.versions.$version",
                                  "rabbit.ssl_options.versions",
                                  [{datatype, atom}, {collect, list}]})
    ],
    Conf = [],
    Result = cuttlefish_generator:map({[], Mappings, []}, Conf),
    %% {collect, list} with no matches → key absent from app.config.
    ?assertEqual([], Result),
    ok.

rabbit_ssl_versions_absent_assert(Result) ->
    case Result of
        [] -> ok;
        [{rabbit, Opts}] ->
            case proplists:get_value(ssl_options, Opts) of
                undefined -> ok;
                SslOpts ->
                    case proplists:get_value(versions, SslOpts) of
                        undefined -> ok;
                        []        -> ok;
                        Other     -> error({unexpected_versions, Other})
                    end
            end
    end.

%%
%% Pattern 2: auth_mechanisms.$name → rabbit.auth_mechanisms
%%
%% From rabbit.schema lines 484-492. Collects SASL mechanism atoms into a
%% sorted list. The translation uses lists:keysort/2; {collect, list} sorts
%% by the $name segment, which is the last component — identical ordering.
%%

%% Current form — explicit translation, exactly as in rabbit.schema.
auth_mechanisms_current_form_test() ->
    Mappings = [
        cuttlefish_mapping:parse({mapping, "auth_mechanisms.$name",
                                  "rabbit.auth_mechanisms",
                                  [{datatype, atom}]})
    ],
    Translations = [
        cuttlefish_translation:parse(
            {translation, "rabbit.auth_mechanisms",
             fun(Conf) ->
                 Settings = cuttlefish_variable:filter_by_prefix(
                              "auth_mechanisms", Conf),
                 Sorted = lists:keysort(1, Settings),
                 [V || {_, V} <- Sorted]
             end})
    ],
    Conf = [
        {["auth_mechanisms", "PLAIN"],     'PLAIN'},
        {["auth_mechanisms", "AMQPLAIN"], 'AMQPLAIN'}
    ],
    Result = cuttlefish_generator:map({Translations, Mappings, []}, Conf),
    ?assertMatch([{rabbit, [{auth_mechanisms, _}]}], Result),
    [{rabbit, [{auth_mechanisms, Mechanisms}]}] = Result,
    %% keysort on ["auth_mechanisms","AMQPLAIN"] vs ["auth_mechanisms","PLAIN"]:
    %% "AMQPLAIN" < "PLAIN" lexicographically.
    ?assertEqual(['AMQPLAIN', 'PLAIN'], Mechanisms),
    ok.

%% New form — {collect, list} produces identical sorted output.
auth_mechanisms_collect_form_test() ->
    Mappings = [
        cuttlefish_mapping:parse({mapping, "auth_mechanisms.$name",
                                  "rabbit.auth_mechanisms",
                                  [{datatype, atom}, {collect, list}]})
    ],
    Conf = [
        {["auth_mechanisms", "PLAIN"],     'PLAIN'},
        {["auth_mechanisms", "AMQPLAIN"], 'AMQPLAIN'}
    ],
    Result = cuttlefish_generator:map({[], Mappings, []}, Conf),
    ?assertMatch([{rabbit, [{auth_mechanisms, _}]}], Result),
    [{rabbit, [{auth_mechanisms, Mechanisms}]}] = Result,
    %% {collect, list} sorts by $name segment: "AMQPLAIN" < "PLAIN".
    ?assertEqual(['AMQPLAIN', 'PLAIN'], Mechanisms),
    ok.

%%
%% Pattern 3: deprecated_features.permit.$name → rabbit.permit_deprecated_features
%%
%% From rabbit.schema lines 2325-2343. Collects feature flag enable/disable
%% settings into a map keyed by atom. The current translation builds a map
%% using maps:from_list/1; {collect, {map, atom}} does the same automatically.
%%

%% Current form — explicit translation, exactly as in rabbit.schema.
deprecated_features_current_form_test() ->
    Mappings = [
        cuttlefish_mapping:parse(
            {mapping, "deprecated_features.permit.$name",
             "rabbit.permit_deprecated_features",
             [{datatype, {enum, [true, false]}}]})
    ],
    Translations = [
        cuttlefish_translation:parse(
            {translation, "rabbit.permit_deprecated_features",
             fun(Conf) ->
                 Settings = cuttlefish_variable:filter_by_prefix(
                              "deprecated_features.permit", Conf),
                 maps:from_list(
                   [{list_to_atom(FeatureName), State}
                    || {["deprecated_features", "permit", FeatureName], State}
                       <- Settings])
             end})
    ],
    Conf = [
        {["deprecated_features", "permit", "classic_queue_mirroring"], false},
        {["deprecated_features", "permit", "transient_nonexcl_queues"], true}
    ],
    Result = cuttlefish_generator:map({Translations, Mappings, []}, Conf),
    ?assertMatch([{rabbit, [{permit_deprecated_features, _}]}], Result),
    [{rabbit, [{permit_deprecated_features, M}]}] = Result,
    ?assert(is_map(M)),
    ?assertEqual(false, maps:get(classic_queue_mirroring, M)),
    ?assertEqual(true,  maps:get(transient_nonexcl_queues, M)),
    ok.

%% New form — {collect, {map, atom}} replaces the translation entirely.
deprecated_features_collect_form_test() ->
    Mappings = [
        cuttlefish_mapping:parse(
            {mapping, "deprecated_features.permit.$name",
             "rabbit.permit_deprecated_features",
             [{datatype, {enum, [true, false]}}, {collect, {map, atom}}]})
    ],
    Conf = [
        {["deprecated_features", "permit", "classic_queue_mirroring"], false},
        {["deprecated_features", "permit", "transient_nonexcl_queues"], true}
    ],
    Result = cuttlefish_generator:map({[], Mappings, []}, Conf),
    ?assertMatch([{rabbit, [{permit_deprecated_features, _}]}], Result),
    [{rabbit, [{permit_deprecated_features, M}]}] = Result,
    ?assert(is_map(M)),
    ?assertEqual(false, maps:get(classic_queue_mirroring, M)),
    ?assertEqual(true,  maps:get(transient_nonexcl_queues, M)),
    ok.

%%
%% Backwards-compatibility: existing schemas with translations are unaffected
%% when the mapping has no {collect, ...} property. The translation always
%% takes precedence when both a translation and a collect mapping exist for
%% the same Erlang target.
%%

translation_always_takes_precedence_test() ->
    %% Mapping declares {collect, list}, but a translation for the same
    %% Erlang target also exists. The translation must win.
    Mappings = [
        cuttlefish_mapping:parse({mapping, "auth_mechanisms.$name",
                                  "rabbit.auth_mechanisms",
                                  [{datatype, atom}, {collect, list}]})
    ],
    %% Translation returns a hard-coded sentinel to make detection easy.
    Translations = [
        cuttlefish_translation:parse(
            {translation, "rabbit.auth_mechanisms",
             fun(_Conf) -> [sentinel_value] end})
    ],
    Conf = [
        {["auth_mechanisms", "PLAIN"], 'PLAIN'}
    ],
    Result = cuttlefish_generator:map({Translations, Mappings, []}, Conf),
    ?assertMatch([{rabbit, [{auth_mechanisms, [sentinel_value]}]}], Result),
    ok.
