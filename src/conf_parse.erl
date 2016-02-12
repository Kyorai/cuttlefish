-module(conf_parse).
-export([parse/1,file/1]).
-define(p_anything,true).
-define(p_charclass,true).
-define(p_choose,true).
-define(p_label,true).
-define(p_not,true).
-define(p_one_or_more,true).
-define(p_optional,true).
-define(p_scan,true).
-define(p_seq,true).
-define(p_string,true).
-define(p_zero_or_more,true).




%% -------------------------------------------------------------------
%%
%% conf_parse: for all your .conf parsing needs.
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

%% This module implements the parser for a sysctl-style
%% configuration format. Example:
%%
%% ```
%% riak.local.node = riak@127.0.0.1
%% riak.local.http = 127.0.0.1:8098
%% riak.local.pb = 127.0.0.1:8087
%% riak.local.storage.backend = bitcask'''
%%
%% This would parse into the following flat proplist:
%%
%% ```
%% [{<<"riak.local.node">>,<<"riak@127.0.0.1">>},
%% {<<"riak.local.http">>,<<"127.0.0.1:8098">>},
%% {<<"riak.local.pb">>,<<"127.0.0.1:8087">>},
%% {<<"riak.local.storage.backend">>,<<"bitcask">>}]'''
%%
%% Other modules in this application interpret and validate the
%% result of a successful parse.
%% @end
-define(line, true).
-define(FMT(F,A), lists:flatten(io_lib:format(F,A))).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

%% @doc Only let through lines that are not comments or whitespace.
is_setting(ws) -> false;
is_setting(comment) -> false;
is_setting(_) -> true.

%% @doc Removes escaped dots from keys
unescape_dots([$\\,$.|Rest]) ->
    [$.|unescape_dots(Rest)];
unescape_dots([]) -> [];
unescape_dots([C|Rest]) ->
    [C|unescape_dots(Rest)].

-ifdef(TEST).
file_test() ->
    Conf = conf_parse:file("../test/riak.conf"),
    ?assertEqual([
            {["ring_size"],"32"},
            {["anti_entropy"],"debug"},
            {["log","error","file"],"/var/log/error.log"},
            {["log","console","file"],"/var/log/console.log"},
            {["log","syslog"],"on"},
            {["listener","http","internal"],"127.0.0.1:8098"},
            {["listener","http","external"],"10.0.0.1:80"}
        ], Conf),
    ok.

utf8_test() ->
    Conf = conf_parse:parse("setting = thingŒ\n"),
    ?assertEqual([{["setting"],
            {error, {conf_to_latin1, 1}}
        }], Conf),
    ok.
-endif.

-spec file(file:name()) -> any().
file(Filename) -> case file:read_file(Filename) of {ok,Bin} -> parse(Bin); Err -> Err end.

-spec parse(binary() | list()) -> any().
parse(List) when is_list(List) -> parse(list_to_binary(List));
parse(Input) when is_binary(Input) ->
  _ = setup_memo(),
  Result = case 'config'(Input,{{line,1},{column,1}}) of
             {AST, <<>>, _Index} -> AST;
             Any -> Any
           end,
  release_memo(), Result.

-spec 'config'(input(), index()) -> parse_result().
'config'(Input, Index) ->
  p(Input, Index, 'config', fun(I,D) -> (p_zero_or_more(fun 'line'/2))(I,D) end, fun(Node, _Idx) ->
    [ L || L <- Node, is_setting(L) ]
 end).

-spec 'line'(input(), index()) -> parse_result().
'line'(Input, Index) ->
  p(Input, Index, 'line', fun(I,D) -> (p_choose([p_seq([p_choose([fun 'setting'/2, fun 'comment'/2, p_one_or_more(fun 'ws'/2)]), p_choose([fun 'crlf'/2, fun 'eof'/2])]), fun 'crlf'/2]))(I,D) end, fun(Node, _Idx) ->
    case Node of
        [ Line, _EOL ] -> Line;
        Line -> Line
    end
 end).

-spec 'setting'(input(), index()) -> parse_result().
'setting'(Input, Index) ->
  p(Input, Index, 'setting', fun(I,D) -> (p_seq([p_zero_or_more(fun 'ws'/2), fun 'key'/2, p_zero_or_more(fun 'ws'/2), p_string(<<"=">>), p_zero_or_more(fun 'ws'/2), fun 'value'/2, p_zero_or_more(fun 'ws'/2), p_optional(fun 'comment'/2)]))(I,D) end, fun(Node, _Idx) ->
    [ _, Key, _, _Eq, _, Value, _, _ ] = Node,
    {Key, Value}
 end).

-spec 'key'(input(), index()) -> parse_result().
'key'(Input, Index) ->
  p(Input, Index, 'key', fun(I,D) -> (p_seq([p_label('head', fun 'word'/2), p_label('tail', p_zero_or_more(p_seq([p_string(<<".">>), fun 'word'/2])))]))(I,D) end, fun(Node, _Idx) ->
    [{head, H}, {tail, T}] = Node,
    [unicode:characters_to_list(H)| [ unicode:characters_to_list(W) || [_, W] <- T]]
 end).

-spec 'value'(input(), index()) -> parse_result().
'value'(Input, Index) ->
  p(Input, Index, 'value', fun(I,D) -> (p_one_or_more(p_seq([p_not(p_choose([p_seq([p_zero_or_more(fun 'ws'/2), fun 'crlf'/2]), fun 'comment'/2])), p_anything()])))(I,D) end, fun(Node, Idx) ->
    case unicode:characters_to_binary(Node, utf8, latin1) of
        {_Status, _Begining, _Rest} ->
            {error, {conf_to_latin1, line(Idx)}};
        Bin ->
            binary_to_list(Bin)
    end
 end).

-spec 'comment'(input(), index()) -> parse_result().
'comment'(Input, Index) ->
  p(Input, Index, 'comment', fun(I,D) -> (p_seq([p_zero_or_more(fun 'ws'/2), p_string(<<"#">>), p_zero_or_more(p_seq([p_not(fun 'crlf'/2), p_anything()]))]))(I,D) end, fun(_Node, _Idx) ->comment end).

-spec 'word'(input(), index()) -> parse_result().
'word'(Input, Index) ->
  p(Input, Index, 'word', fun(I,D) -> (p_one_or_more(p_choose([p_string(<<"\\.">>), p_charclass(<<"[A-Za-z0-9_-]">>)])))(I,D) end, fun(Node, _Idx) ->
    unescape_dots(unicode:characters_to_list(Node))
 end).

-spec 'crlf'(input(), index()) -> parse_result().
'crlf'(Input, Index) ->
  p(Input, Index, 'crlf', fun(I,D) -> (p_seq([p_optional(p_string(<<"\r">>)), p_string(<<"\n">>)]))(I,D) end, fun(_Node, _Idx) ->ws end).

-spec 'eof'(input(), index()) -> parse_result().
'eof'(Input, Index) ->
  p(Input, Index, 'eof', fun(I,D) -> (p_not(p_anything()))(I,D) end, fun(_Node, _Idx) ->ws end).

-spec 'ws'(input(), index()) -> parse_result().
'ws'(Input, Index) ->
  p(Input, Index, 'ws', fun(I,D) -> (p_charclass(<<"[\s\t]">>))(I,D) end, fun(_Node, _Idx) ->ws end).



-file("peg_includes.hrl", 1).
-type index() :: {{line, pos_integer()}, {column, pos_integer()}}.
-type input() :: binary().
-type parse_failure() :: {fail, term()}.
-type parse_success() :: {term(), input(), index()}.
-type parse_result() :: parse_failure() | parse_success().
-type parse_fun() :: fun((input(), index()) -> parse_result()).
-type xform_fun() :: fun((input(), index()) -> term()).

-spec p(input(), index(), atom(), parse_fun(), xform_fun()) -> parse_result().
p(Inp, StartIndex, Name, ParseFun, TransformFun) ->
  case get_memo(StartIndex, Name) of      % See if the current reduction is memoized
    {ok, Memo} -> %Memo;                     % If it is, return the stored result
      Memo;
    _ ->                                        % If not, attempt to parse
      Result = case ParseFun(Inp, StartIndex) of
        {fail,_} = Failure ->                       % If it fails, memoize the failure
          Failure;
        {Match, InpRem, NewIndex} ->               % If it passes, transform and memoize the result.
          Transformed = TransformFun(Match, StartIndex),
          {Transformed, InpRem, NewIndex}
      end,
      memoize(StartIndex, Name, Result),
      Result
  end.

-spec setup_memo() -> ets:tid().
setup_memo() ->
  put({parse_memo_table, ?MODULE}, ets:new(?MODULE, [set])).

-spec release_memo() -> true.
release_memo() ->
  ets:delete(memo_table_name()).

-spec memoize(index(), atom(), parse_result()) -> true.
memoize(Index, Name, Result) ->
  Memo = case ets:lookup(memo_table_name(), Index) of
              [] -> [];
              [{Index, Plist}] -> Plist
         end,
  ets:insert(memo_table_name(), {Index, [{Name, Result}|Memo]}).

-spec get_memo(index(), atom()) -> {ok, term()} | {error, not_found}.
get_memo(Index, Name) ->
  case ets:lookup(memo_table_name(), Index) of
    [] -> {error, not_found};
    [{Index, Plist}] ->
      case proplists:lookup(Name, Plist) of
        {Name, Result}  -> {ok, Result};
        _  -> {error, not_found}
      end
    end.

-spec memo_table_name() -> ets:tid().
memo_table_name() ->
    get({parse_memo_table, ?MODULE}).

-ifdef(p_eof).
-spec p_eof() -> parse_fun().
p_eof() ->
  fun(<<>>, Index) -> {eof, [], Index};
     (_, Index) -> {fail, {expected, eof, Index}} end.
-endif.

-ifdef(p_optional).
-spec p_optional(parse_fun()) -> parse_fun().
p_optional(P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} -> {[], Input, Index};
        {_, _, _} = Success -> Success
      end
  end.
-endif.

-ifdef(p_not).
-spec p_not(parse_fun()) -> parse_fun().
p_not(P) ->
  fun(Input, Index)->
      case P(Input,Index) of
        {fail,_} ->
          {[], Input, Index};
        {Result, _, _} -> {fail, {expected, {no_match, Result},Index}}
      end
  end.
-endif.

-ifdef(p_assert).
-spec p_assert(parse_fun()) -> parse_fun().
p_assert(P) ->
  fun(Input,Index) ->
      case P(Input,Index) of
        {fail,_} = Failure-> Failure;
        _ -> {[], Input, Index}
      end
  end.
-endif.

-ifdef(p_seq).
-spec p_seq([parse_fun()]) -> parse_fun().
p_seq(P) ->
  fun(Input, Index) ->
      p_all(P, Input, Index, [])
  end.

-spec p_all([parse_fun()], input(), index(), [term()]) -> parse_result().
p_all([], Inp, Index, Accum ) -> {lists:reverse( Accum ), Inp, Index};
p_all([P|Parsers], Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail, _} = Failure -> Failure;
    {Result, InpRem, NewIndex} -> p_all(Parsers, InpRem, NewIndex, [Result|Accum])
  end.
-endif.

-ifdef(p_choose).
-spec p_choose([parse_fun()]) -> parse_fun().
p_choose(Parsers) ->
  fun(Input, Index) ->
      p_attempt(Parsers, Input, Index, none)
  end.

-spec p_attempt([parse_fun()], input(), index(), none | parse_failure()) -> parse_result().
p_attempt([], _Input, _Index, Failure) -> Failure;
p_attempt([P|Parsers], Input, Index, FirstFailure)->
  case P(Input, Index) of
    {fail, _} = Failure ->
      case FirstFailure of
        none -> p_attempt(Parsers, Input, Index, Failure);
        _ -> p_attempt(Parsers, Input, Index, FirstFailure)
      end;
    Result -> Result
  end.
-endif.

-ifdef(p_zero_or_more).
-spec p_zero_or_more(parse_fun()) -> parse_fun().
p_zero_or_more(P) ->
  fun(Input, Index) ->
      p_scan(P, Input, Index, [])
  end.
-endif.

-ifdef(p_one_or_more).
-spec p_one_or_more(parse_fun()) -> parse_fun().
p_one_or_more(P) ->
  fun(Input, Index)->
      Result = p_scan(P, Input, Index, []),
      case Result of
        {[_|_], _, _} ->
          Result;
        _ ->
          {fail, {expected, Failure, _}} = P(Input,Index),
          {fail, {expected, {at_least_one, Failure}, Index}}
      end
  end.
-endif.

-ifdef(p_label).
-spec p_label(atom(), parse_fun()) -> parse_fun().
p_label(Tag, P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} = Failure ->
           Failure;
        {Result, InpRem, NewIndex} ->
          {{Tag, Result}, InpRem, NewIndex}
      end
  end.
-endif.

-ifdef(p_scan).
-spec p_scan(parse_fun(), input(), index(), [term()]) -> {[term()], input(), index()}.
p_scan(_, <<>>, Index, Accum) -> {lists:reverse(Accum), <<>>, Index};
p_scan(P, Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail,_} -> {lists:reverse(Accum), Inp, Index};
    {Result, InpRem, NewIndex} -> p_scan(P, InpRem, NewIndex, [Result | Accum])
  end.
-endif.

-ifdef(p_string).
-spec p_string(binary()) -> parse_fun().
p_string(S) ->
    Length = erlang:byte_size(S),
    fun(Input, Index) ->
      try
          <<S:Length/binary, Rest/binary>> = Input,
          {S, Rest, p_advance_index(S, Index)}
      catch
          error:{badmatch,_} -> {fail, {expected, {string, S}, Index}}
      end
    end.
-endif.

-ifdef(p_anything).
-spec p_anything() -> parse_fun().
p_anything() ->
  fun(<<>>, Index) -> {fail, {expected, any_character, Index}};
     (Input, Index) when is_binary(Input) ->
          <<C/utf8, Rest/binary>> = Input,
          {<<C/utf8>>, Rest, p_advance_index(<<C/utf8>>, Index)}
  end.
-endif.

-ifdef(p_charclass).
-spec p_charclass(string() | binary()) -> parse_fun().
p_charclass(Class) ->
    {ok, RE} = re:compile(Class, [unicode, dotall]),
    fun(Inp, Index) ->
            case re:run(Inp, RE, [anchored]) of
                {match, [{0, Length}|_]} ->
                    {Head, Tail} = erlang:split_binary(Inp, Length),
                    {Head, Tail, p_advance_index(Head, Index)};
                _ -> {fail, {expected, {character_class, binary_to_list(Class)}, Index}}
            end
    end.
-endif.

-ifdef(p_regexp).
-spec p_regexp(binary()) -> parse_fun().
p_regexp(Regexp) ->
    {ok, RE} = re:compile(Regexp, [unicode, dotall, anchored]),
    fun(Inp, Index) ->
        case re:run(Inp, RE) of
            {match, [{0, Length}|_]} ->
                {Head, Tail} = erlang:split_binary(Inp, Length),
                {Head, Tail, p_advance_index(Head, Index)};
            _ -> {fail, {expected, {regexp, binary_to_list(Regexp)}, Index}}
        end
    end.
-endif.

-ifdef(line).
-spec line(index() | term()) -> pos_integer() | undefined.
line({{line,L},_}) -> L;
line(_) -> undefined.
-endif.

-ifdef(column).
-spec column(index() | term()) -> pos_integer() | undefined.
column({_,{column,C}}) -> C;
column(_) -> undefined.
-endif.

-spec p_advance_index(input() | unicode:charlist() | pos_integer(), index()) -> index().
p_advance_index(MatchedInput, Index) when is_list(MatchedInput) orelse is_binary(MatchedInput)-> % strings
  lists:foldl(fun p_advance_index/2, Index, unicode:characters_to_list(MatchedInput));
p_advance_index(MatchedInput, Index) when is_integer(MatchedInput) -> % single characters
  {{line, Line}, {column, Col}} = Index,
  case MatchedInput of
    $\n -> {{line, Line+1}, {column, 1}};
    _ -> {{line, Line}, {column, Col+1}}
  end.
