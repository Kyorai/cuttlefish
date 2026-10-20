%% Copyright (c) 2026 Cuttlefish maintainers.  All Rights Reserved.
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
-module(cuttlefish_encrypted_datatype_tests).

-include_lib("eunit/include/eunit.hrl").

-define(XLATE(X), lists:flatten(cuttlefish_error:xlate(X))).

-define(URL_CIPHERTEXT,
        "WZh80oR2wzUYhpQNCiXxhR759We0uQeMs4jm8FS6M2OXbi/h0OrnZPzT85H93MRZ15mnQrpgYJV+83bXUk9wYlU0we0TT9BMCD8rfecbnNU=").
-define(PASSWORD_CIPHERTEXT,
        "ec3Z4yFBjnzh+7VeDZ+tRoB7LFI2ZnU8H8eyIingNwV8d1q/DQ/cF4rnZXzwgszlLd0ZkEz5WaMZoNa8n50dqA==").

-define(PLAINTEXT_VALUES, [
    "https://ca.example.com/certs",
    "https://rabbitmq-definitions.s3.eu-west-1.amazonaws.com/prod/definitions.json",
    "s3://rabbitmq-definitions/prod/definitions.json",
    "urn:amazon:webservices",
    "urn:amazon:cognito:sp:eu-west-1_Ab12CdEfG",
    "urn:ietf:params:oauth:grant-type:token-exchange",
    "arn:aws:secretsmanager:eu-west-1:123456789012:secret:rabbitmq/default-pass-Ab12Cd",
    "AKIAIOSFODNN7EXAMPLE",
    "svc:rabbitmq",
    "hunter2:x"
]).

-define(MALFORMED_ENCRYPTED_VALUES, [
    "Encrypted:" ++ ?PASSWORD_CIPHERTEXT,
    "encrypted" ++ ?PASSWORD_CIPHERTEXT,
    "encrypted:"
]).

encrypted_string_accepts_ciphertext_test() ->
    ?assertEqual({encrypted, ?URL_CIPHERTEXT},
                 cuttlefish_datatypes:from_string("encrypted:" ++ ?URL_CIPHERTEXT,
                                                  encrypted_string)).

encrypted_binary_accepts_ciphertext_test() ->
    ?assertEqual({encrypted, list_to_binary(?PASSWORD_CIPHERTEXT)},
                 cuttlefish_datatypes:from_string("encrypted:" ++ ?PASSWORD_CIPHERTEXT,
                                                  encrypted_binary)).

encrypted_value_is_split_at_the_first_colon_test() ->
    ?assertEqual({encrypted, "https://ca.example.com/certs"},
                 cuttlefish_datatypes:from_string("encrypted:https://ca.example.com/certs",
                                                  encrypted_string)),
    ?assertEqual({encrypted, <<"urn:amazon:webservices">>},
                 cuttlefish_datatypes:from_string("encrypted:urn:amazon:webservices",
                                                  encrypted_binary)).

encrypted_types_reject_plaintext_values_test() ->
    [?assertEqual({error, {encrypted_value_expected, Type}},
                  cuttlefish_datatypes:from_string(Value, Type))
     || Type <- [encrypted_string, encrypted_binary], Value <- ?PLAINTEXT_VALUES].

encrypted_types_reject_malformed_encrypted_values_test() ->
    [?assertEqual({error, {encrypted_value_expected, Type}},
                  cuttlefish_datatypes:from_string(Value, Type))
     || Type <- [encrypted_string, encrypted_binary], Value <- ?MALFORMED_ENCRYPTED_VALUES].

encrypted_types_reject_non_list_values_test() ->
    [?assertEqual({error, {encrypted_value_expected, Type}},
                  cuttlefish_datatypes:from_string(Value, Type))
     || Type <- [encrypted_string, encrypted_binary],
        Value <- [list_to_binary(V) || V <- ?PLAINTEXT_VALUES]].

encrypted_value_expected_error_omits_non_list_value_test() ->
    Error = cuttlefish_datatypes:from_string(<<"urn:amazon:cognito:sp:eu-west-1_Ab12CdEfG">>,
                                             encrypted_binary),
    ?assertEqual(nomatch, string:find(?XLATE(Error), "cognito")).

encrypted_value_expected_error_omits_the_value_test() ->
    Error = cuttlefish_datatypes:from_string("hunter2:x", encrypted_binary),
    ?assertEqual("Value must be prefixed with \"encrypted:\"", ?XLATE(Error)).

encrypted_types_to_string_test() ->
    ?assertEqual("encrypted:" ++ ?URL_CIPHERTEXT,
                 cuttlefish_datatypes:to_string({encrypted, ?URL_CIPHERTEXT},
                                                encrypted_string)),
    ?assertEqual("encrypted:" ++ ?PASSWORD_CIPHERTEXT,
                 cuttlefish_datatypes:to_string({encrypted, list_to_binary(?PASSWORD_CIPHERTEXT)},
                                                encrypted_binary)).

generated_conf_renders_a_plaintext_default_test() ->
    {_, Mappings, _} = cuttlefish_schema:strings(["
{mapping, \"default_pass\", \"rabbit.default_pass\", [
    {datatype, [encrypted_binary, binary]},
    {default, <<\"guest\">>}
]}.
{mapping, \"definitions.https.url\", \"rabbit.definitions_url\", [
    {datatype, [encrypted_string, {uri, [https]}]},
    {default, \"https://definitions.example.com/rabbitmq.json\"}
]}.
"]),
    Lines = cuttlefish_conf:generate(Mappings),
    ?assert(lists:member("default_pass = guest", Lines)),
    ?assert(lists:member("definitions.https.url = https://definitions.example.com/rabbitmq.json",
                         Lines)),
    ?assert(lists:member("## Default: guest", Lines)).

encrypted_types_are_supported_test() ->
    ?assert(cuttlefish_datatypes:is_supported(encrypted_string)),
    ?assert(cuttlefish_datatypes:is_supported(encrypted_binary)),
    ?assert(cuttlefish_datatypes:is_valid_list([encrypted_string, {uri, [https]}])),
    ?assert(cuttlefish_datatypes:is_valid_list([encrypted_binary, binary])).

-define(SCHEMA, "
{mapping, \"default_pass\", \"rabbit.default_pass\", [
    {datatype, [encrypted_binary, binary]}
]}.

{mapping, \"cluster_formation.aws.access_key_id\",
          \"rabbit.aws_access_key\", [
    {datatype, [encrypted_string, string]}
]}.

{mapping, \"trust_store.url\", \"rabbitmq_trust_store.url\", [
    {datatype, [encrypted_string, {uri, [https]}]}
]}.

{mapping, \"management.oauth_token_endpoint_params.$name\",
          \"rabbitmq_management.oauth_token_endpoint_params\", [
    {datatype, [encrypted_binary, binary]},
    {collect, {map, binary}}
]}.

{mapping, \"auth_ldap.dn_lookup_bind.password\",
          \"rabbitmq_auth_backend_ldap.dn_lookup_bind_password\", [
    {datatype, encrypted_binary}
]}.
").

generate(ConfLines) ->
    Schema = cuttlefish_schema:strings([?SCHEMA]),
    Conf = conf_parse:parse(list_to_binary([[L, "\n"] || L <- ConfLines])),
    cuttlefish_generator:map(Schema, Conf).

app_env(App, Key, Config) ->
    proplists:get_value(Key, proplists:get_value(App, Config)).

encrypted_values_in_a_conf_file_test() ->
    Config = generate(["default_pass = encrypted:" ++ ?PASSWORD_CIPHERTEXT,
                       "trust_store.url = encrypted:" ++ ?URL_CIPHERTEXT]),
    ?assertEqual({encrypted, list_to_binary(?PASSWORD_CIPHERTEXT)},
                 app_env(rabbit, default_pass, Config)),
    ?assertEqual({encrypted, ?URL_CIPHERTEXT},
                 app_env(rabbitmq_trust_store, url, Config)).

plaintext_values_in_a_conf_file_test() ->
    Config = generate(["default_pass = hunter2:x",
                       "cluster_formation.aws.access_key_id = AKIAIOSFODNN7EXAMPLE",
                       "trust_store.url = https://ca.example.com/certs"]),
    ?assertEqual(<<"hunter2:x">>, app_env(rabbit, default_pass, Config)),
    ?assertEqual("AKIAIOSFODNN7EXAMPLE", app_env(rabbit, aws_access_key, Config)),
    ?assertEqual("https://ca.example.com/certs",
                 app_env(rabbitmq_trust_store, url, Config)).

plaintext_uri_is_still_validated_test() ->
    ?assertMatch({error, transform_datatypes, _},
                 generate(["trust_store.url = http://ca.example.com/certs"])).

mixed_endpoint_params_in_a_conf_file_test() ->
    Config = generate(
               ["management.oauth_token_endpoint_params.audience = urn:amazon:cognito:sp:eu-west-1_Ab12CdEfG",
                "management.oauth_token_endpoint_params.resource = https://rabbitmq.eu-west-1.example.com",
                "management.oauth_token_endpoint_params.client_assertion = encrypted:" ++ ?PASSWORD_CIPHERTEXT]),
    ?assertEqual(#{<<"audience">> => <<"urn:amazon:cognito:sp:eu-west-1_Ab12CdEfG">>,
                   <<"resource">> => <<"https://rabbitmq.eu-west-1.example.com">>,
                   <<"client_assertion">> => {encrypted, list_to_binary(?PASSWORD_CIPHERTEXT)}},
                 app_env(rabbitmq_management, oauth_token_endpoint_params, Config)).

encrypted_only_mapping_rejects_plaintext_test() ->
    ?assertEqual({encrypted, list_to_binary(?PASSWORD_CIPHERTEXT)},
                 app_env(rabbitmq_auth_backend_ldap, dn_lookup_bind_password,
                         generate(["auth_ldap.dn_lookup_bind.password = encrypted:" ++
                                       ?PASSWORD_CIPHERTEXT]))),
    {error, transform_datatypes, {errorlist, Errors}} =
        generate(["auth_ldap.dn_lookup_bind.password = Pa55w0rd-rabbitmq"]),
    Messages = lists:flatten([?XLATE(E) || E <- Errors]),
    ?assertNotEqual(nomatch, string:find(Messages, "prefixed with \"encrypted:\"")),
    ?assertEqual(nomatch, string:find(Messages, "Pa55w0rd-rabbitmq")).

pretty_datatype_test() ->
    ?assertEqual("an encrypted value, e.g. encrypted:<ciphertext>",
                 cuttlefish_conf:pretty_datatype(encrypted_binary)).
