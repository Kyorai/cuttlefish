-module(cuttlefish_alias_proper_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PROP(P), ?assert(proper:quickcheck(P, [{numtests, 500}, {to_file, user}]))).

%%
%% Test entry points
%%

idempotent_test() -> ?PROP(prop_idempotent()).
length_test()     -> ?PROP(prop_length()).
no_alias_keys_survive_test() -> ?PROP(prop_no_alias_keys_survive()).
canonical_preserved_test()   -> ?PROP(prop_canonical_preserved()).
alias_value_promoted_test()  -> ?PROP(prop_alias_value_promoted()).

multi_alias_idempotent_test()          -> ?PROP(prop_multi_alias_idempotent()).
multi_alias_no_alias_keys_survive_test() -> ?PROP(prop_multi_alias_no_alias_keys_survive()).
multi_alias_first_wins_test()          -> ?PROP(prop_multi_alias_first_wins()).

%% Properties: aliases with custom messages
msg_idempotent_test()             -> ?PROP(prop_msg_idempotent()).
msg_no_alias_keys_survive_test()  -> ?PROP(prop_msg_no_alias_keys_survive()).
msg_canonical_preserved_test()    -> ?PROP(prop_msg_canonical_preserved()).
msg_alias_value_promoted_test()   -> ?PROP(prop_msg_alias_value_promoted()).
msg_messages_preserved_test()     -> ?PROP(prop_msg_messages_preserved()).

%%
%% Properties: single alias per mapping
%%

%% Alias resolution is idempotent: after the first pass all aliases are
%% gone, so a second pass is a no-op.
prop_idempotent() ->
    ?FORALL({Conf, Mappings}, gen_conf_with_aliases(),
        begin
            Once = cuttlefish_generator:resolve_aliases(Conf, Mappings),
            Twice = cuttlefish_generator:resolve_aliases(Once, Mappings),
            Once =:= Twice
        end).

%% Alias resolution never grows the conf. It rewrites keys (length
%% unchanged) or drops them (length shrinks). It never adds entries.
prop_length() ->
    ?FORALL({Conf, Mappings}, gen_conf_with_aliases(),
        begin
            Resolved = cuttlefish_generator:resolve_aliases(Conf, Mappings),
            length(Resolved) =< length(Conf)
        end).

%% No alias keys survive resolution.
prop_no_alias_keys_survive() ->
    ?FORALL({Conf, Mappings}, gen_conf_with_aliases(),
        begin
            Resolved = cuttlefish_generator:resolve_aliases(Conf, Mappings),
            AllAliases = lists:flatmap(
                fun cuttlefish_mapping:aliases/1, Mappings),
            ResolvedKeys = [K || {K, _} <- Resolved],
            lists:all(
                fun(A) -> not lists:member(A, ResolvedKeys) end,
                AllAliases)
        end).

%% If the canonical key is present in the input, it is present with the
%% same value in the output.
prop_canonical_preserved() ->
    ?FORALL({Conf, Mappings}, gen_conf_with_aliases(),
        begin
            Resolved = cuttlefish_generator:resolve_aliases(Conf, Mappings),
            lists:all(fun(Mapping) ->
                Canonical = cuttlefish_mapping:variable(Mapping),
                case proplists:get_value(Canonical, Conf) of
                    undefined -> true;
                    Val -> proplists:get_value(Canonical, Resolved) =:= Val
                end
            end, Mappings)
        end).

%% When only an alias is set (canonical absent), the resolved conf
%% contains the alias's value under the canonical key.
prop_alias_value_promoted() ->
    ?FORALL({Conf, Mappings}, gen_conf_with_aliases(),
        begin
            Resolved = cuttlefish_generator:resolve_aliases(Conf, Mappings),
            lists:all(fun(Mapping) ->
                Canonical = cuttlefish_mapping:variable(Mapping),
                [Alias] = cuttlefish_mapping:aliases(Mapping),
                CanonicalInInput = proplists:get_value(Canonical, Conf),
                AliasInInput = proplists:get_value(Alias, Conf),
                case {CanonicalInInput, AliasInInput} of
                    {undefined, undefined} -> true;
                    {undefined, AliasVal} ->
                        proplists:get_value(Canonical, Resolved) =:= AliasVal;
                    {_, _} -> true %% canonical-wins tested by prop_canonical_preserved
                end
            end, Mappings)
        end).

%%
%% Properties: multiple aliases per mapping
%%

prop_multi_alias_idempotent() ->
    ?FORALL({Conf, Mappings}, gen_conf_with_multi_aliases(),
        begin
            Once = cuttlefish_generator:resolve_aliases(Conf, Mappings),
            Twice = cuttlefish_generator:resolve_aliases(Once, Mappings),
            Once =:= Twice
        end).

prop_multi_alias_no_alias_keys_survive() ->
    ?FORALL({Conf, Mappings}, gen_conf_with_multi_aliases(),
        begin
            Resolved = cuttlefish_generator:resolve_aliases(Conf, Mappings),
            AllAliases = lists:flatmap(
                fun cuttlefish_mapping:aliases/1, Mappings),
            ResolvedKeys = [K || {K, _} <- Resolved],
            lists:all(
                fun(A) -> not lists:member(A, ResolvedKeys) end,
                AllAliases)
        end).

%% When canonical is not set and multiple aliases are, the first alias
%% in declaration order wins.
prop_multi_alias_first_wins() ->
    ?FORALL({Conf, Mappings}, gen_conf_with_multi_aliases(),
        begin
            Resolved = cuttlefish_generator:resolve_aliases(Conf, Mappings),
            lists:all(fun(Mapping) ->
                Canonical = cuttlefish_mapping:variable(Mapping),
                Aliases = cuttlefish_mapping:aliases(Mapping),
                CanonicalInInput = proplists:get_value(Canonical, Conf),
                case CanonicalInInput of
                    undefined ->
                        %% Find the first set alias
                        FirstSetAlias = lists:dropwhile(
                            fun(A) -> proplists:get_value(A, Conf) =:= undefined end,
                            Aliases),
                        case FirstSetAlias of
                            [] -> true;
                            [Winner|_] ->
                                ExpectedVal = proplists:get_value(Winner, Conf),
                                proplists:get_value(Canonical, Resolved) =:= ExpectedVal
                        end;
                    _ -> true
                end
            end, Mappings)
        end).

%%
%% Properties: aliases with custom messages
%%

prop_msg_idempotent() ->
    ?FORALL({Conf, Mappings}, gen_conf_with_msg_aliases(),
        begin
            Once = cuttlefish_generator:resolve_aliases(Conf, Mappings),
            Twice = cuttlefish_generator:resolve_aliases(Once, Mappings),
            Once =:= Twice
        end).

prop_msg_no_alias_keys_survive() ->
    ?FORALL({Conf, Mappings}, gen_conf_with_msg_aliases(),
        begin
            Resolved = cuttlefish_generator:resolve_aliases(Conf, Mappings),
            AllAliases = lists:flatmap(
                fun cuttlefish_mapping:aliases/1, Mappings),
            ResolvedKeys = [K || {K, _} <- Resolved],
            lists:all(
                fun(A) -> not lists:member(A, ResolvedKeys) end,
                AllAliases)
        end).

prop_msg_canonical_preserved() ->
    ?FORALL({Conf, Mappings}, gen_conf_with_msg_aliases(),
        begin
            Resolved = cuttlefish_generator:resolve_aliases(Conf, Mappings),
            lists:all(fun(Mapping) ->
                Canonical = cuttlefish_mapping:variable(Mapping),
                case proplists:get_value(Canonical, Conf) of
                    undefined -> true;
                    Val -> proplists:get_value(Canonical, Resolved) =:= Val
                end
            end, Mappings)
        end).

prop_msg_alias_value_promoted() ->
    ?FORALL({Conf, Mappings}, gen_conf_with_msg_aliases(),
        begin
            Resolved = cuttlefish_generator:resolve_aliases(Conf, Mappings),
            lists:all(fun(Mapping) ->
                Canonical = cuttlefish_mapping:variable(Mapping),
                [Alias] = cuttlefish_mapping:aliases(Mapping),
                CanonicalInInput = proplists:get_value(Canonical, Conf),
                AliasInInput = proplists:get_value(Alias, Conf),
                case {CanonicalInInput, AliasInInput} of
                    {undefined, undefined} -> true;
                    {undefined, AliasVal} ->
                        proplists:get_value(Canonical, Resolved) =:= AliasVal;
                    {_, _} -> true
                end
            end, Mappings)
        end).

%% A parse and aliases_with_messages round-trip.
prop_msg_messages_preserved() ->
    ?FORALL({_Conf, Mappings, ExpectedMessages}, gen_conf_with_msg_aliases_and_expected(),
        begin
            lists:all(fun({Mapping, ExpMsg}) ->
                [{_Var, ActualMsg}] = cuttlefish_mapping:aliases_with_messages(Mapping),
                ActualMsg =:= ExpMsg
            end, lists:zip(Mappings, ExpectedMessages))
        end).

%%
%% Generators
%%

gen_segment() ->
    ?LET(S, non_empty(list(oneof([choose($a, $z), choose($0, $9)]))),
         S).

gen_conf_key() ->
    ?LET(Segments, non_empty(list(gen_segment())),
         Segments).

%% Which conf entries to include for a single-alias mapping:
%%   canonical only, alias only, both, or neither.
gen_conf_entry_choice() ->
    elements([canonical_only, alias_only, both, neither]).

%% Single-alias generator: each mapping has exactly one alias.
gen_conf_with_aliases() ->
    ?LET(N, range(1, 5),
        ?SUCHTHAT({_Conf, Mappings}, gen_conf_with_aliases_raw(N),
            Mappings =/= [])).

gen_conf_with_aliases_raw(N) ->
    ?LET(Pairs, vector(N, {gen_conf_key(), gen_conf_key(), gen_alias_form()}),
        begin
            {UniquePairs, _Seen} = deduplicate_keys(
                [{C, A} || {C, A, _} <- Pairs]),
            FormMap = maps:from_list([{C, F} || {C, _, F} <- Pairs]),
            Mappings = [cuttlefish_mapping:parse(
                {mapping,
                 cuttlefish_variable:format(Canonical),
                 "app." ++ cuttlefish_variable:format(Canonical),
                 [{datatype, string},
                  alias_prop(maps:get(Canonical, FormMap, plural),
                             cuttlefish_variable:format(Alias))]}
            ) || {Canonical, Alias} <- UniquePairs],
            ?LET(Choices, vector(length(UniquePairs), gen_conf_entry_choice()),
                begin
                    Conf = lists:flatmap(fun({{Canonical, Alias}, Choice}) ->
                        case Choice of
                            canonical_only -> [{Canonical, "canonical_val"}];
                            alias_only     -> [{Alias, "alias_val"}];
                            both           -> [{Canonical, "canonical_val"},
                                               {Alias, "alias_val"}];
                            neither        -> []
                        end
                    end, lists:zip(UniquePairs, Choices)),
                    {Conf, Mappings}
                end)
        end).

%% Multi-alias generator: each mapping has 1-3 aliases.
gen_conf_with_multi_aliases() ->
    ?LET(N, range(1, 3),
        ?SUCHTHAT({_Conf, Mappings}, gen_conf_with_multi_aliases_raw(N),
            Mappings =/= [])).

gen_alias_keys() ->
    ?LET(Count, range(1, 3),
        vector(Count, gen_conf_key())).

gen_conf_with_multi_aliases_raw(N) ->
    ?LET(Groups, vector(N, {gen_conf_key(), gen_alias_keys()}),
        begin
            %% Flatten all keys (canonical + all aliases) for dedup
            {UniqueGroups, _Seen} = lists:foldl(fun({C, As}, {Acc, Seen}) ->
                AllKeys = [C | As],
                HasCollision = lists:any(fun(K) -> lists:member(K, Seen) end, AllKeys)
                    orelse length(lists:usort(AllKeys)) =/= length(AllKeys),
                case HasCollision of
                    true  -> {Acc, Seen};
                    false -> {[{C, As} | Acc], AllKeys ++ Seen}
                end
            end, {[], []}, Groups),

            Mappings = [cuttlefish_mapping:parse(
                {mapping,
                 cuttlefish_variable:format(Canonical),
                 "app." ++ cuttlefish_variable:format(Canonical),
                 [{datatype, string},
                  {aliases, [cuttlefish_variable:format(A) || A <- Aliases]}]}
            ) || {Canonical, Aliases} <- UniqueGroups],

            %% Generate a boolean for each key (canonical + each alias)
            %% to decide whether to include it in conf
            AllKeyLists = [begin
                AliasIndexed = lists:zip(Aliases, lists:seq(1, length(Aliases))),
                {Canonical, AliasIndexed}
            end || {Canonical, Aliases} <- UniqueGroups],
            TotalBools = lists:sum([1 + length(As) || {_, As} <- UniqueGroups]),
            ?LET(Bools, vector(TotalBools, boolean()),
                begin
                    {Conf, _} = lists:foldl(fun({Canonical, AliasIndexed}, {ConfAcc, BoolsAcc}) ->
                        [IncludeCanonical | Rest1] = BoolsAcc,
                        {AliasEntries, Rest2} = lists:foldl(
                            fun({A, I}, {Entries, [B|Bs]}) ->
                                case B of
                                    true -> {[{A, "alias_val_" ++ integer_to_list(I)} | Entries], Bs};
                                    false -> {Entries, Bs}
                                end
                            end, {[], Rest1}, AliasIndexed),
                        CanonicalEntries = case IncludeCanonical of
                            true -> [{Canonical, "canonical_val"}];
                            false -> []
                        end,
                        {ConfAcc ++ CanonicalEntries ++ lists:reverse(AliasEntries), Rest2}
                    end, {[], Bools}, AllKeyLists),
                    {Conf, Mappings}
                end)
        end).

gen_conf_with_msg_aliases() ->
    ?LET(N, range(1, 5),
        ?SUCHTHAT({_Conf, Mappings}, gen_conf_with_msg_aliases_raw(N),
            Mappings =/= [])).

gen_optional_message() ->
    oneof([undefined, non_empty(list(oneof([choose($a, $z), $\s, choose($0, $9)])))]).

gen_conf_with_msg_aliases_raw(N) ->
    ?LET(Quads, vector(N, {gen_conf_key(), gen_conf_key(), gen_optional_message(), gen_alias_form()}),
        begin
            Triples = [{C, A, M} || {C, A, M, _} <- Quads],
            FormMap = maps:from_list([{C, F} || {C, _, _, F} <- Quads]),
            {PairsWithMsgs, Mappings} = build_msg_alias_mappings(Triples, FormMap),
            ?LET(Choices, vector(length(PairsWithMsgs), gen_conf_entry_choice()),
                begin
                    Conf = lists:flatmap(fun({{Canonical, Alias, _Msg}, Choice}) ->
                        case Choice of
                            canonical_only -> [{Canonical, "canonical_val"}];
                            alias_only     -> [{Alias, "alias_val"}];
                            both           -> [{Canonical, "canonical_val"},
                                               {Alias, "alias_val"}];
                            neither        -> []
                        end
                    end, lists:zip(PairsWithMsgs, Choices)),
                    {Conf, Mappings}
                end)
        end).

gen_conf_with_msg_aliases_and_expected() ->
    ?LET(N, range(1, 5),
        ?SUCHTHAT({_Conf, Mappings, _Msgs}, gen_conf_with_msg_aliases_and_expected_raw(N),
            Mappings =/= [])).

gen_conf_with_msg_aliases_and_expected_raw(N) ->
    ?LET(Quads, vector(N, {gen_conf_key(), gen_conf_key(), gen_optional_message(), gen_alias_form()}),
        begin
            Triples = [{C, A, M} || {C, A, M, _} <- Quads],
            FormMap = maps:from_list([{C, F} || {C, _, _, F} <- Quads]),
            {PairsWithMsgs, Mappings} = build_msg_alias_mappings(Triples, FormMap),
            ExpectedMessages = [Msg || {_, _, Msg} <- PairsWithMsgs],
            {[], Mappings, ExpectedMessages}
        end).

build_msg_alias_mappings(Triples, FormMap) ->
    %% Deduplicate, keeping messages attached to their canonical-alias pairs
    {PairsWithMsgs, _Seen} = lists:foldl(fun({C, A, Msg}, {Acc, Seen}) ->
        case lists:member(C, Seen) orelse
             lists:member(A, Seen) orelse
             C =:= A of
            true  -> {Acc, Seen};
            false -> {[{C, A, Msg} | Acc], [C, A | Seen]}
        end
    end, {[], []}, Triples),
    Mappings = [cuttlefish_mapping:parse(
        {mapping,
         cuttlefish_variable:format(Canonical),
         "app." ++ cuttlefish_variable:format(Canonical),
         [{datatype, string},
          alias_prop(maps:get(Canonical, FormMap, plural),
                    alias_entry(Alias, Msg))]}
    ) || {Canonical, Alias, Msg} <- PairsWithMsgs],
    {PairsWithMsgs, Mappings}.

alias_entry(Alias, undefined) ->
    cuttlefish_variable:format(Alias);
alias_entry(Alias, Msg) ->
    {cuttlefish_variable:format(Alias), Msg}.

gen_alias_form() ->
    elements([singular, plural]).

alias_prop(singular, Entry) -> {alias, Entry};
alias_prop(plural, Entry)   -> {aliases, [Entry]}.

%% Helpers

deduplicate_keys(Pairs) ->
    lists:foldl(fun({C, A}, {Acc, Seen}) ->
        case lists:member(C, Seen) orelse
             lists:member(A, Seen) orelse
             C =:= A of
            true  -> {Acc, Seen};
            false -> {[{C, A} | Acc], [C, A | Seen]}
        end
    end, {[], []}, Pairs).
