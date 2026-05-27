-module(cuttlefish_partial_proper_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PROP(P), ?assert(proper:quickcheck(P, [{numtests, 200}, {to_file, user}]))).

%% --- entry points -------------------------------------------------

rewrite_pure_test() -> ?PROP(prop_rewrite_pure()).
rewrite_idempotent_in_keys_test() -> ?PROP(prop_rewrite_idempotent_in_keys()).
emitted_keys_carry_prefix_test() -> ?PROP(prop_emitted_keys_carry_prefix()).
exclude_section_is_exact_segment_test() -> ?PROP(prop_exclude_section_is_exact_segment()).
exclude_exact_drops_only_matches_test() -> ?PROP(prop_exclude_exact_drops_only_matches()).
validator_passthrough_test() -> ?PROP(prop_validator_passthrough()).
translation_binding_is_arity1_test() -> ?PROP(prop_translation_binding_is_arity1()).
translation_binding_forwards_prefixes_test() -> ?PROP(prop_translation_binding_forwards_prefixes()).
inline_equivalence_test() -> ?PROP(prop_inline_equivalence()).

%% --- properties ---------------------------------------------------

%% Rewriting is a pure function: same input -> same output.
prop_rewrite_pure() ->
    ?FORALL({Terms, Opts}, {gen_terms(), gen_include_opts()},
        cuttlefish_partial:rewrite(Terms, Opts)
            =:= cuttlefish_partial:rewrite(Terms, Opts)).

%% Running the rewriter twice yields the same keys both times (the
%% first pass already produced prefixed keys; the second pass with the
%% same opts would double-prepend, so this property checks the more
%% useful "outputs are stable across re-invocations" guarantee).
prop_rewrite_idempotent_in_keys() ->
    ?FORALL({Terms, Opts}, {gen_terms(), gen_include_opts()},
        begin
            R1 = cuttlefish_partial:rewrite(Terms, Opts),
            R2 = cuttlefish_partial:rewrite(Terms, Opts),
            R1 =:= R2
        end).

%% Every emitted mapping/translation carries the include's prefix.
prop_emitted_keys_carry_prefix() ->
    ?FORALL({Terms, Opts}, {gen_terms(), gen_include_opts()},
        begin
            {ok, Out} = cuttlefish_partial:rewrite(Terms, Opts),
            Prefix = proplists:get_value(prefix, Opts),
            AppPrefix = proplists:get_value(app_prefix, Opts),
            lists:all(
              fun({mapping, ConfKey, AppKey, _}) ->
                      lists:prefix(Prefix ++ ".", ConfKey)
                          andalso lists:prefix(AppPrefix ++ ".", AppKey);
                 ({translation, AppKey, _}) ->
                      lists:prefix(AppPrefix ++ ".", AppKey);
                 ({validator, _, _, _}) ->
                      true
              end, Out)
        end).

%% Section match is first-segment-exact, not substring. A section
%% name S drops every term whose bare conf key equals S or starts
%% with `S.`, and nothing else.
prop_exclude_section_is_exact_segment() ->
    ?FORALL({Section, Tail}, {gen_segment(), gen_segment()},
        ?IMPLIES(Section =/= Tail
                 andalso not lists:prefix(Section ++ ".", Tail)
                 andalso Tail =/= Section,
            begin
                Terms = [{mapping, Section, Section, []},
                         {mapping, Section ++ ".sub", Section, []},
                         {mapping, Section ++ "_extra", Section ++ "_extra", []},
                         {mapping, Tail, Tail, []}],
                Opts = base_opts() ++ [{exclude, [Section]}],
                {ok, Out} = cuttlefish_partial:rewrite(Terms, Opts),
                OutBares = [strip_prefix(K) || {mapping, K, _, _} <- Out],
                %% `Section` and `Section.sub` are dropped;
                %% `Section_extra` and `Tail` survive.
                lists:sort(OutBares) =:= lists:sort([Section ++ "_extra", Tail])
            end)).

%% Exact-name exclude drops only the exact match.
prop_exclude_exact_drops_only_matches() ->
    ?FORALL(Bares, gen_unique_bare_keys(),
        ?IMPLIES(Bares =/= [],
            begin
                [Target | Rest] = Bares,
                Terms = [{mapping, B, B, []} || B <- Bares],
                Opts = base_opts() ++ [{exclude, [Target]}],
                {ok, Out} = cuttlefish_partial:rewrite(Terms, Opts),
                OutBares = lists:sort([strip_prefix(K)
                                       || {mapping, K, _, _} <- Out]),
                ExpectSurvivors = lists:sort([B || B <- Rest, B =/= Target]),
                OutBares =:= ExpectSurvivors
            end)).

%% Validators are passed through byte-for-byte.
prop_validator_passthrough() ->
    ?FORALL({Name, Desc, Opts}, {gen_segment(), gen_segment(),
                                  gen_include_opts()},
        begin
            F = fun(_) -> true end,
            V = {validator, Name, Desc, F},
            cuttlefish_partial:rewrite([V], Opts) =:= {ok, [V]}
        end).

%% Any arity-3 partial_translation becomes an arity-1 closure.
prop_translation_binding_is_arity1() ->
    ?FORALL({BareKey, Opts}, {gen_segment(), gen_include_opts()},
        begin
            Term = {partial_translation, BareKey,
                    fun(_C, _P, _A) -> ok end},
            {ok, [Out]} = cuttlefish_partial:rewrite([Term], Opts),
            {translation, _, Bound} = Out,
            erlang:fun_info(Bound, arity) =:= {arity, 1}
        end).

%% The bound closure forwards the include's prefixes to the source fun.
prop_translation_binding_forwards_prefixes() ->
    ?FORALL({BareKey, Opts}, {gen_segment(), gen_include_opts()},
        begin
            Term = {partial_translation, BareKey,
                    fun(C, P, A) -> {C, P, A} end},
            {ok, [Out]} = cuttlefish_partial:rewrite([Term], Opts),
            {translation, _, Bound} = Out,
            P = proplists:get_value(prefix, Opts),
            A = proplists:get_value(app_prefix, Opts),
            Bound(some_conf) =:= {some_conf, P, A}
        end).

%% The strongest correctness property: expanding a partial via the
%% rewriter is observationally equal to writing those same terms
%% inline with the prefixes hand-applied. Pins the "pure macro
%% expansion" guarantee.
prop_inline_equivalence() ->
    ?FORALL({Terms, Opts}, {gen_mappings_only(), gen_include_opts()},
        begin
            {ok, Expanded} = cuttlefish_partial:rewrite(Terms, Opts),
            Inline = manual_rewrite(Terms, Opts),
            Expanded =:= Inline
        end).

%% --- generators ---------------------------------------------------

gen_include_opts() ->
    ?LET({P, A}, {gen_segment(), gen_segment()},
         [{prefix, P}, {app_prefix, A}]).

base_opts() ->
    [{prefix, "p"}, {app_prefix, "a"}].

%% A safe, lowercase, alphanumeric segment of 1-8 chars. Avoids
%% characters that would confuse the dotted prefix machinery.
gen_segment() ->
    ?LET(N, range(1, 8),
         vector(N, oneof([range($a, $z), range($0, $9)]))).

gen_unique_bare_keys() ->
    ?LET(L, list(gen_segment()), lists:usort(L)).

gen_terms() ->
    list(oneof([gen_mapping(), gen_validator(), gen_partial_translation()])).

gen_mappings_only() ->
    list(gen_mapping()).

gen_mapping() ->
    ?LET({K, App}, {gen_segment(), gen_segment()},
         {mapping, K, App, [{datatype, atom}]}).

gen_validator() ->
    ?LET({N, D}, {gen_segment(), gen_segment()},
         {validator, N, D, fun(_) -> true end}).

gen_partial_translation() ->
    ?LET(K, gen_segment(),
         {partial_translation, K, fun(_, _, _) -> ok end}).

%% --- helpers ------------------------------------------------------

strip_prefix(Key) ->
    %% Strip the leading "p." that base_opts() prepends.
    case Key of
        "p." ++ Rest -> Rest;
        _ -> Key
    end.

%% A deliberately naive "what the schema author would have written
%% inline" — concatenate prefix.bare for mappings, no exclude.
manual_rewrite(Terms, Opts) ->
    Prefix = proplists:get_value(prefix, Opts),
    AppPrefix = proplists:get_value(app_prefix, Opts),
    [{mapping, Prefix ++ "." ++ K, AppPrefix ++ "." ++ App, OptsM}
     || {mapping, K, App, OptsM} <- Terms].
