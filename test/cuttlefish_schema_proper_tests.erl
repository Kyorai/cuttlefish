-module(cuttlefish_schema_proper_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PROP(P), {timeout, 60, ?_assert(proper:quickcheck(P, [{to_file, user}, {numtests, 200}]))}).

list_schemas_only_returns_schema_extension_test_() ->
    ?PROP(prop_list_schemas_only_schema_extension()).

list_schemas_returns_sorted_test_() ->
    ?PROP(prop_list_schemas_sorted()).

list_schemas_ignores_dotfiles_test_() ->
    ?PROP(prop_list_schemas_ignores_dotfiles()).

file_refuses_non_schema_extensions_test_() ->
    ?PROP(prop_non_schema_extension_refused()).

%% --- Properties ---

prop_list_schemas_only_schema_extension() ->
    ?FORALL(Names, list(filename_gen()),
            begin
                Dir = make_dir_with(Names),
                try
                    All = cuttlefish_schema:list_schemas(Dir),
                    lists:all(fun(P) ->
                                      lists:suffix(".schema",
                                                   filename:basename(P))
                              end, All)
                after
                    cleanup_dir(Dir)
                end
            end).

prop_list_schemas_sorted() ->
    ?FORALL(Names, list(filename_gen()),
            begin
                Dir = make_dir_with(Names),
                try
                    Got = cuttlefish_schema:list_schemas(Dir),
                    Got =:= lists:sort(Got)
                after
                    cleanup_dir(Dir)
                end
            end).

prop_list_schemas_ignores_dotfiles() ->
    ?FORALL(Names, list(filename_gen()),
            begin
                Dir = make_dir_with(Names),
                try
                    Got = cuttlefish_schema:list_schemas(Dir),
                    lists:all(fun(P) ->
                                      Base = filename:basename(P),
                                      not lists:prefix(".", Base)
                              end, Got)
                after
                    cleanup_dir(Dir)
                end
            end).

prop_non_schema_extension_refused() ->
    %% Any filename that does not end in `.schema' must surface as
    %% `not_a_schema_file' without ever reaching `erl_scan'.
    ?FORALL(Name, non_schema_filename_gen(),
            begin
                Result = cuttlefish_schema:files([Name]),
                case Result of
                    {errorlist, [{error, {schema_file, _,
                                          {error, {not_a_schema_file, _}}}}]} ->
                        true;
                    _ ->
                        false
                end
            end).

%% --- Generators ---

filename_gen() ->
    %% A mixture of normal schema files, non-schema files, dotfiles,
    %% and editor junk.
    oneof([
        ?LET(B, basename_chars(), B ++ ".schema"),
        ?LET(B, basename_chars(), B ++ ".conf"),
        ?LET(B, basename_chars(), B ++ ".md"),
        ?LET(B, basename_chars(), "." ++ B),
        ?LET(B, basename_chars(), "." ++ B ++ ".schema"),
        ?LET(B, basename_chars(), B ++ ".schema~")
    ]).

non_schema_filename_gen() ->
    oneof([
        ?LET(B, basename_chars(), B ++ ".conf"),
        ?LET(B, basename_chars(), B ++ ".dump"),
        ?LET(B, basename_chars(), B ++ ".md"),
        ?LET(B, basename_chars(), B ++ ".swp"),
        ?LET(B, basename_chars(), B ++ "~"),
        ?LET(B, basename_chars(), B)
    ]).

basename_chars() ->
    %% Letters and digits only, length 1..10, to keep generated names
    %% safe on every filesystem.
    ?LET(L, integer(1, 10),
         vector(L, oneof([integer($a, $z), integer($A, $Z), integer($0, $9)]))).

%% --- Helpers ---

make_dir_with(Names) ->
    Dir = tmp_dir("schema-prop-"),
    lists:foreach(
      fun(N) ->
              %% Skip ".", "..", and any name with a slash to keep mkfile safe.
              case is_safe_name(N) of
                  true ->
                      _ = file:write_file(filename:join(Dir, N), <<"">>);
                  false ->
                      ok
              end
      end, Names),
    Dir.

is_safe_name(N) ->
    N =/= "." andalso N =/= ".." andalso not lists:member($/, N).

tmp_dir(Prefix) ->
    Base = case os:getenv("TMPDIR") of
               false -> "/tmp";
               T -> T
           end,
    Unique = integer_to_list(erlang:unique_integer([positive])),
    Dir = filename:join(Base, Prefix ++ Unique),
    ok = file:make_dir(Dir),
    Dir.

cleanup_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:foreach(
              fun(F) -> file:delete(filename:join(Dir, F)) end,
              Files);
        _ -> ok
    end,
    file:del_dir(Dir).
