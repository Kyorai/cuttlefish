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

%% Values with no usable `encrypted:' prefix at all. A prefix that is
%% present but has nothing after it is a different fault, covered by
%% ?BLANK_CIPHERTEXTS below.
-define(MALFORMED_ENCRYPTED_VALUES, [
    "Encrypted:" ++ ?PASSWORD_CIPHERTEXT,
    "encrypted" ++ ?PASSWORD_CIPHERTEXT
]).

-define(BLANK_CIPHERTEXTS, [
    "encrypted:",
    "encrypted: ",
    "encrypted:   ",
    "encrypted:\t",
    "encrypted:\t  "
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

%% A prefix with nothing usable after it is not a missing prefix. Telling
%% the operator to add a prefix that is already there sends them looking in
%% the wrong place, so this fault gets its own error and its own message.
encrypted_types_report_a_blank_ciphertext_as_blank_test() ->
    [?assertEqual({error, {encrypted_value_empty, Type}},
                  cuttlefish_datatypes:from_string(Value, Type))
     || Type <- [encrypted_string, encrypted_binary], Value <- ?BLANK_CIPHERTEXTS].

blank_ciphertext_message_does_not_ask_for_the_prefix_test() ->
    Message = ?XLATE({encrypted_value_empty, encrypted_binary}),
    ?assertEqual("Value is prefixed with \"encrypted:\" but the ciphertext "
                 "after it is empty", Message).

encrypted_types_reject_non_list_values_test() ->
    [?assertEqual({error, {encrypted_value_expected, Type}},
                  cuttlefish_datatypes:from_string(Value, Type))
     || Type <- [encrypted_string, encrypted_binary],
        Value <- [list_to_binary(V) || V <- ?PLAINTEXT_VALUES]].

encrypted_value_expected_message_is_actionable_test() ->
    Error = cuttlefish_datatypes:from_string("hunter2:x", encrypted_binary),
    ?assertEqual("Value must be prefixed with \"encrypted:\"", ?XLATE(Error)).

encrypted_types_to_string_test() ->
    ?assertEqual("encrypted:" ++ ?URL_CIPHERTEXT,
                 cuttlefish_datatypes:to_string({encrypted, ?URL_CIPHERTEXT},
                                                encrypted_string)),
    ?assertEqual("encrypted:" ++ ?PASSWORD_CIPHERTEXT,
                 cuttlefish_datatypes:to_string({encrypted, list_to_binary(?PASSWORD_CIPHERTEXT)},
                                                encrypted_binary)).

-define(PLAINTEXT_DEFAULT_SCHEMA, "
{mapping, \"default_pass\", \"rabbit.default_pass\", [
    {datatype, [encrypted_binary, binary]},
    {default, <<\"guest\">>}
]}.
{mapping, \"definitions.https.url\", \"rabbit.definitions_url\", [
    {datatype, [encrypted_string, {uri, [https]}]},
    {default, \"https://definitions.example.com/rabbitmq.json\"}
]}.
").

generated_conf_renders_a_plaintext_default_test() ->
    {_, Mappings, _} = cuttlefish_schema:strings([?PLAINTEXT_DEFAULT_SCHEMA]),
    Lines = cuttlefish_conf:generate(Mappings),
    ?assert(lists:member("default_pass = guest", Lines)),
    ?assert(lists:member("definitions.https.url = https://definitions.example.com/rabbitmq.json",
                         Lines)),
    ?assert(lists:member("## Default: guest", Lines)).

%% The schema above is the combination REFERENCE.md documents. Rendering it
%% into a .conf file is only half the contract: the same defaults have to
%% survive the pipeline, or `generate' writes a file cuttlefish then refuses
%% to read.
plaintext_defaults_also_survive_the_pipeline_test() ->
    Config = cuttlefish_unit:generate_config(
               strings, [?PLAINTEXT_DEFAULT_SCHEMA], []),
    cuttlefish_unit:assert_config(Config, "rabbit.default_pass", <<"guest">>),
    cuttlefish_unit:assert_config(Config, "rabbit.definitions_url",
                                 "https://definitions.example.com/rabbitmq.json").

%% A default already in its native form has to pass back through unchanged,
%% because `add_defaults/2' injects defaults as raw terms and
%% `transform_datatypes/3' then runs over them. Every other non-string
%% datatype (ip, integer, domain_socket, ...) has such a clause.
encrypted_native_default_is_accepted_test() ->
    Config = cuttlefish_unit:generate_config(
               strings,
               ["{mapping, \"p\", \"a.p\", [
                     {datatype, encrypted_binary},
                     {default, {encrypted, <<\"CIPHERTEXT\">>}}
                 ]}."],
               []),
    cuttlefish_unit:assert_config(Config, "a.p", {encrypted, <<"CIPHERTEXT">>}),
    StringConfig = cuttlefish_unit:generate_config(
                     strings,
                     ["{mapping, \"p\", \"a.p\", [
                           {datatype, encrypted_string},
                           {default, {encrypted, \"CIPHERTEXT\"}}
                       ]}."],
                     []),
    cuttlefish_unit:assert_config(StringConfig, "a.p", {encrypted, "CIPHERTEXT"}).

%% A blank ciphertext in the native form is the same fault as in the string
%% form, and reports the same error.
encrypted_native_default_rejects_a_blank_ciphertext_test() ->
    [?assertEqual({error, {encrypted_value_empty, encrypted_string}},
                  cuttlefish_datatypes:from_string({encrypted, V}, encrypted_string))
     || V <- ["", "  ", "\t"]],
    [?assertEqual({error, {encrypted_value_empty, encrypted_binary}},
                  cuttlefish_datatypes:from_string({encrypted, V}, encrypted_binary))
     || V <- [<<>>, <<"  ">>, <<"\t">>, "", "  "]].

%% A default has to be rendered by the datatype that accepts it, not by the
%% head of the datatype list. `encrypted_string' cannot render a bare string,
%% so the `{uri, _}' behind it owns this value.
generated_conf_renders_a_default_owned_by_a_later_datatype_test() ->
    {_, Mappings, _} = cuttlefish_schema:strings(
                         ["{mapping, \"u\", \"a.u\", [
                               {datatype, [encrypted_string, {uri, [https]}]},
                               {default, \"https://x/y\"}
                           ]}."]),
    Lines = cuttlefish_conf:generate(Mappings),
    ?assert(lists:member("u = https://x/y", Lines)),
    ?assert(lists:member("## Default: https://x/y", Lines)).

%% Same selection, reached through `commented' rather than `default'.
generated_conf_renders_a_commented_value_owned_by_a_later_datatype_test() ->
    {_, Mappings, _} = cuttlefish_schema:strings(
                         ["{mapping, \"u\", \"a.u\", [
                               {datatype, [encrypted_string, {uri, [https]}]},
                               {commented, \"https://x/y\"}
                           ]}."]),
    ?assert(lists:member("## u = https://x/y", cuttlefish_conf:generate(Mappings))).

%% `{uri, _}' accepts only strings, so no datatype here can render a binary.
%% Rendering it anyway is what produced the `badarg' crash in `generate/1'.
generated_conf_omits_a_commented_value_no_datatype_can_render_test() ->
    ok = cuttlefish_test_logging:set_up(),
    ok = cuttlefish_test_logging:bounce(warning),
    {_, Mappings, _} = cuttlefish_schema:strings(
                         ["{mapping, \"u\", \"a.u\", [
                               {datatype, [encrypted_string, {uri, [https]}]},
                               {commented, <<\"https://x/y\">>}
                           ]}."]),
    Rendered = lists:flatten(cuttlefish_conf:generate(Mappings)),
    ?assertEqual(nomatch, string:find(Rendered, "u = ")),
    Logs = lists:flatten(cuttlefish_test_logging:get_logs()),
    ?assertNotEqual(nomatch, string:find(Logs, "none of its datatypes")).

%% An encrypted-only mapping whose default is plaintext is a schema bug. The
%% one thing `generate' must not do is emit a setting line that cuttlefish
%% then rejects when it reads the file back.
encrypted_only_mapping_with_a_plaintext_default_emits_no_setting_test() ->
    ok = cuttlefish_test_logging:set_up(),
    ok = cuttlefish_test_logging:bounce(warning),
    {_, Mappings, _} = cuttlefish_schema:strings(
                         ["{mapping, \"p\", \"a.p\", [
                               {datatype, encrypted_binary},
                               {default, \"guest\"}
                           ]}."]),
    Rendered = lists:flatten(cuttlefish_conf:generate(Mappings)),
    ?assertEqual(nomatch, string:find(Rendered, "p = guest")),
    Logs = lists:flatten(cuttlefish_test_logging:get_logs()),
    ?assertNotEqual(nomatch, string:find(Logs, "none of its datatypes")).

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

%% Whitespace is not a secret. A quoted value preserves it, unlike a bare
%% one, so without a trim the application receives `{encrypted, <<"   ">>}'
%% as though it were valid ciphertext.
whitespace_only_ciphertext_is_rejected_through_the_pipeline_test() ->
    ?assertMatch({error, transform_datatypes, _},
                 generate(["auth_ldap.dn_lookup_bind.password = 'encrypted:   '"])).

%% The redaction in `redact_if_sensitive/3' elides any error not known to be
%% value-free, so a new error atom reaches the operator as the generic
%% summary unless it is listed. Assert the accurate message survives.
blank_ciphertext_message_survives_redaction_test() ->
    {error, transform_datatypes, {errorlist, Errors}} =
        generate(["auth_ldap.dn_lookup_bind.password = encrypted:"]),
    Messages = lists:flatten([?XLATE(E) || E <- Errors]),
    ?assertNotEqual(nomatch, string:find(Messages, "the ciphertext after it is empty")).

%% Every datatype in a mapping's list contributes an error when the value is
%% rejected, so redacting only the encrypted error is not enough: the sibling
%% `{uri, _}' reports the value it rejected. Nothing about a setting that may
%% hold a secret may carry the value, in the error terms or in the rendered
%% messages.
sensitive_mapping_errors_never_contain_the_value_test() ->
    Secret = "hunter2-TOPSECRET",
    {error, transform_datatypes, {errorlist, Errors}} =
        generate(["trust_store.url = " ++ Secret]),
    Terms = lists:flatten(io_lib:format("~p", [Errors])),
    ?assertEqual(nomatch, string:find(Terms, Secret)),
    Messages = lists:flatten([?XLATE(E) || E <- Errors]),
    ?assertEqual(nomatch, string:find(Messages, Secret)),
    %% The operator still has to be told which setting was rejected.
    ?assertNotEqual(nomatch, string:find(Messages, "trust_store.url")).

%% The redaction is scoped to mappings that may hold a secret. A mapping with
%% no encrypted datatype keeps its specific, value-bearing diagnostic.
non_sensitive_mapping_errors_still_name_the_value_test() ->
    Config = cuttlefish_unit:generate_config(
               strings,
               ["{mapping, \"plain.url\", \"a.url\", [{datatype, {uri, [https]}}]}."],
               [{["plain", "url"], "not-a-url"}]),
    {error, transform_datatypes, {errorlist, Errors}} = Config,
    Messages = lists:flatten([?XLATE(E) || E <- Errors]),
    ?assertNotEqual(nomatch, string:find(Messages, "not-a-url")).

%% An encrypted datatype behind a total one is dead code: `binary' and
%% `string' parse anything, including an `encrypted:...' value, so the
%% earlier datatype always wins and the ciphertext reaches the application
%% as though it were the plaintext secret, with no error and no warning.
%% REFERENCE.md states the ordering rule; this enforces it.
schema_rejects_an_encrypted_datatype_behind_another_test() ->
    ?assertMatch({errorlist, [{error, {encrypted_datatype_not_first, _}}]},
                 cuttlefish_schema:strings(
                   ["{mapping, \"p\", \"a.p\", [{datatype, [binary, encrypted_binary]}]}."])),
    ?assertMatch({errorlist, [{error, {encrypted_datatype_not_first, _}}]},
                 cuttlefish_schema:strings(
                   ["{mapping, \"u\", \"a.u\", [{datatype, [{uri, [https]}, encrypted_string]}]}."])).

schema_accepts_an_encrypted_datatype_first_test() ->
    ?assertMatch({_, [_], _},
                 cuttlefish_schema:strings(
                   ["{mapping, \"p\", \"a.p\", [{datatype, [encrypted_binary, binary]}]}."])),
    ?assertMatch({_, [_], _},
                 cuttlefish_schema:strings(
                   ["{mapping, \"p\", \"a.p\", [{datatype, encrypted_binary}]}."])).

%% Consecutive encrypted datatypes leak nothing: whichever parses first
%% wins and both withhold their value. The rule is that no other datatype
%% may precede an encrypted one, not that exactly one may be first.
schema_accepts_consecutive_encrypted_datatypes_test() ->
    ?assertMatch({_, [_], _},
                 cuttlefish_schema:strings(
                   ["{mapping, \"p\", \"a.p\", [
                         {datatype, [encrypted_string, encrypted_binary]}
                     ]}."])),
    ?assertMatch({_, [_], _},
                 cuttlefish_schema:strings(
                   ["{mapping, \"p\", \"a.p\", [
                         {datatype, [encrypted_string, encrypted_binary, binary]}
                     ]}."])).

%% The rule applies only to mappings that declare an encrypted datatype.
%% Ordering among other datatypes is unconstrained, as it always was.
schema_does_not_constrain_order_without_an_encrypted_datatype_test() ->
    ?assertMatch({_, [_], _},
                 cuttlefish_schema:strings(
                   ["{mapping, \"p\", \"a.p\", [{datatype, [string, integer]}]}."])).

-define(CRASHING_VALIDATOR_SCHEMA,
        "{validator, \"non_empty\", \"must be non-empty\","
        "  fun(B) -> byte_size(B) > 0 end}.\n"
        "{mapping, \"p\", \"a.p\","
        "  [{datatype, [encrypted_binary, binary]},"
        "   {validators, [\"non_empty\"]}]}.\n").

%% Combining an encrypted datatype with a plaintext one changes the shape a
%% validator sees: `{encrypted, Bin}' rather than a bare binary. A validator
%% written for the plaintext shape then raises, and unlike a translation it
%% is applied with no try/catch, so the exception escapes `map_validate/2'
%% and takes the escript down with exit 127 and no cuttlefish error at all.
crashing_validator_is_reported_not_raised_test() ->
    Config = cuttlefish_unit:generate_config(
               strings, [?CRASHING_VALIDATOR_SCHEMA], [{["p"], "encrypted:CIPHERTEXT"}]),
    cuttlefish_unit:assert_error(Config),
    {errorlist, Errors} = element(3, Config),
    Messages = lists:flatten([?XLATE(E) || E <- Errors]),
    ?assertNotEqual(nomatch, string:find(Messages, "p")).

%% A validator that returns false still reports the ordinary validation
%% error, and one that passes still passes. Wrapping the call must not turn
%% either into an exception report.
working_validators_are_unaffected_test() ->
    Schema = "{validator, \"non_empty\", \"must be non-empty\","
             "  fun(B) -> byte_size(B) > 0 end}.\n"
             "{mapping, \"p\", \"a.p\","
             "  [{datatype, binary}, {validators, [\"non_empty\"]}]}.\n",
    cuttlefish_unit:assert_config(
      cuttlefish_unit:generate_config(strings, [Schema], [{["p"], "ok"}]),
      "a.p", <<"ok">>),
    cuttlefish_unit:assert_error_message(
      cuttlefish_unit:generate_config(strings, [Schema], [{["p"], ""}]),
      "p invalid, must be non-empty").

%% Both types, because a guard covering only one would silently document
%% every mapping of the other as the bare word "text".
pretty_datatype_test() ->
    ?assertEqual("an encrypted value, e.g. encrypted:<ciphertext>",
                 cuttlefish_conf:pretty_datatype(encrypted_binary)),
    ?assertEqual("an encrypted value, e.g. encrypted:<ciphertext>",
                 cuttlefish_conf:pretty_datatype(encrypted_string)).
