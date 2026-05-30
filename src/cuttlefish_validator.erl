%% -------------------------------------------------------------------
%%
%% cuttlefish_validator: models a cuttlefish validator
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
-module(cuttlefish_validator).

-record(validator, {
    name::string(),
    description::string(),
    func::fun(),
    aliases = [] :: [string()],
    deprecated :: undefined | {string(), string()}
    }).
-type validator() :: #validator{}.
-type validator_fun() :: fun((any()) -> boolean()).
-type validator_opt() :: {aliases, [string()]}
                       | {deprecated, string(), string()}.
-type raw_validator() :: {validator, string(), string(), validator_fun()}
                       | {validator, string(), string(), validator_fun(),
                          [validator_opt()]}.
-export_type([validator/0]).

-export([
    parse/1,
    parse_and_merge/2,
    is_validator/1,
    name/1,
    description/1,
    func/1,
    aliases/1,
    deprecated/1,
    replace/2,
    matches_name/2]).

-spec parse(raw_validator()) -> validator() | cuttlefish_error:error().
parse({validator, Name, Description, Fun}) ->
    #validator{name = Name, description = Description, func = Fun};
parse({validator, Name, Description, Fun, Opts}) when is_list(Opts) ->
    case parse_opts(Name, Opts) of
        {error, _} = E -> E;
        {Aliases, Deprecated} ->
            #validator{name = Name,
                       description = Description,
                       func = Fun,
                       aliases = Aliases,
                       deprecated = Deprecated}
    end;
parse({validator, Name, _Description, _Fun, BadOpts}) ->
    {error, {validator_options_invalid, Name, BadOpts}};
parse(X) ->
    {error, {validator_parse, X}}.

%% Reject anything that isn't `aliases' or `deprecated' so a typo in
%% the options proplist surfaces at parse time.
parse_opts(Name, Opts) ->
    case lists:partition(fun is_known_opt/1, Opts) of
        {_, [Bad | _]} ->
            {error, {validator_options_invalid, Name, Bad}};
        {Known, []} ->
            case extract_aliases(Name, Known) of
                {error, _} = E -> E;
                Aliases ->
                    case extract_deprecated(Name, Known) of
                        {error, _} = E -> E;
                        Deprecated     -> {Aliases, Deprecated}
                    end
            end
    end.

is_known_opt({aliases, _})       -> true;
is_known_opt({deprecated, _, _}) -> true;
is_known_opt(_)                  -> false.

extract_aliases(Name, Opts) ->
    case proplists:get_value(aliases, Opts) of
        undefined -> [];
        [] -> [];
        L when is_list(L) ->
            case lists:all(fun erlang:is_list/1, L) of
                true ->
                    case length(lists:usort(L)) < length(L) of
                        true  -> {error, {validator_aliases_contain_duplicates, Name}};
                        false -> L
                    end;
                false ->
                    Bad = hd([X || X <- L, not is_list(X)]),
                    {error, {validator_alias_not_a_string, Name, Bad}}
            end;
        Bad -> {error, {validator_alias_not_a_string, Name, Bad}}
    end.

extract_deprecated(Name, Opts) ->
    case [D || {deprecated, _, _} = D <- Opts] of
        [] -> undefined;
        [{deprecated, Since, Hint}] when is_list(Since), is_list(Hint) ->
            {Since, Hint};
        [Bad | _] ->
            {error, {validator_deprecated_malformed, Name, Bad}}
    end.

%% Assumes a foldl over schema elements where each name appears at
%% most once in `Validators', so a single keyreplace handles update.
-spec parse_and_merge(raw_validator(), [validator()]) -> [validator()].
parse_and_merge({validator, ValidatorName, _, _} = ValidatorSource, Validators) ->
    do_parse_and_merge(ValidatorName, ValidatorSource, Validators);
parse_and_merge({validator, ValidatorName, _, _, _} = ValidatorSource, Validators) ->
    do_parse_and_merge(ValidatorName, ValidatorSource, Validators).

do_parse_and_merge(ValidatorName, ValidatorSource, Validators) ->
    %% A parse error must not contaminate the validators list: code
    %% later in the pipeline accesses record fields and would crash
    %% on an `{error, _}' tuple. The schema parser surfaces the
    %% failure through its own error accumulator; here we simply
    %% leave the list unchanged.
    case parse(ValidatorSource) of
        {error, _} ->
            Validators;
        NewValidator ->
            case lists:keyfind(ValidatorName, #validator.name, Validators) of
                false ->
                    [NewValidator | Validators];
                _OldValidator ->
                    lists:keyreplace(ValidatorName, #validator.name,
                                     Validators, NewValidator)
            end
    end.

-spec is_validator(any()) -> boolean().
is_validator(V) -> is_tuple(V) andalso element(1, V) =:= validator.

-spec name(validator()) -> string().
name(V) -> V#validator.name.

-spec description(validator()) -> string().
description(V) -> V#validator.description.

-spec func(validator()) -> fun().
func(V) -> V#validator.func.

-spec aliases(validator()) -> [string()].
aliases(V) -> V#validator.aliases.

-spec deprecated(validator()) -> undefined | {string(), string()}.
deprecated(V) -> V#validator.deprecated.

%% True when `Name' is the canonical name or one of the aliases.
-spec matches_name(string(), validator()) -> boolean().
matches_name(Name, V) ->
    Name =:= V#validator.name orelse lists:member(Name, V#validator.aliases).

-spec replace(validator(), [validator()]) -> [validator()].
replace(Validator, ListOfValidators) ->
    Exists = lists:keymember(name(Validator), #validator.name, ListOfValidators),
    case Exists of
        true ->
            lists:keyreplace(name(Validator), #validator.name, ListOfValidators, Validator);
        _ ->
            [Validator | ListOfValidators]
    end.
