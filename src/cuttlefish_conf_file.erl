%% -------------------------------------------------------------------
%%
%% cuttlefish_conf_file: format-preserving .conf file parser and
%% serializer for config file manipulation and migration.
%%
%% Copyright (c) 2025 Broadcom. All Rights Reserved. The term
%% "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
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
-module(cuttlefish_conf_file).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(conf_file, {
    lines = [] :: [line()]
}).

-type line() ::
    {setting, Key :: string(), Value :: string()} |
    {comment, Text :: string()} |
    {include, Text :: string()} |
    empty.

-opaque conf_file() :: #conf_file{}.
-export_type([conf_file/0, line/0]).

-export([
    load/1,
    parse/1,
    to_string/1,
    save/2,
    get/2,
    set/3,
    remove/2,
    rename_key/3,
    keys/1,
    rename_keys/2,
    remove_keys/2,
    add_comment_before/3,
    comment_out/2
]).

%% ===================================================================
%% Loading and Serialization
%% ===================================================================

-spec load(file:name_all()) -> {ok, conf_file()} | {error, term()}.
load(Filename) ->
    case file:read_file(Filename) of
        {ok, Bin} ->
            Content = unicode:characters_to_list(Bin),
            {ok, parse(Content)};
        {error, _} = Err ->
            Err
    end.

-spec parse(string()) -> conf_file().
parse("") ->
    #conf_file{lines = []};
parse(Content) ->
    Cleaned = strip_utf8_bom(Content),
    RawLines = string:split(Cleaned, "\n", all),
    {Parsed, _} = parse_lines(RawLines, [], normal),
    #conf_file{lines = lists:reverse(Parsed)}.

-spec to_string(conf_file()) -> string().
to_string(#conf_file{lines = Lines}) ->
    lists:flatten(lists:join("\n", [serialize_line(L) || L <- Lines])).

-spec save(conf_file(), file:name_all()) -> ok | {error, term()}.
save(CF, Filename) ->
    file:write_file(Filename, unicode:characters_to_binary(to_string(CF))).

%% ===================================================================
%% Accessors
%% ===================================================================

-spec get(conf_file(), string()) -> {ok, string()} | undefined.
get(#conf_file{lines = Lines}, Key) ->
    find_setting(Lines, Key).

-spec set(conf_file(), string(), string()) -> conf_file().
set(#conf_file{lines = Lines} = CF, Key, Value) ->
    case has_key(Lines, Key) of
        true  -> CF#conf_file{lines = replace_value(Lines, Key, Value)};
        false -> CF#conf_file{lines = Lines ++ [{setting, Key, Value}]}
    end.

-spec remove(conf_file(), string()) -> conf_file().
remove(#conf_file{lines = Lines} = CF, Key) ->
    CF#conf_file{lines = [L || L <- Lines, not is_setting_for(L, Key)]}.

-spec rename_key(conf_file(), string(), string()) -> conf_file().
rename_key(#conf_file{lines = Lines} = CF, OldKey, NewKey) ->
    CF#conf_file{lines = do_rename(Lines, OldKey, NewKey)}.

-spec keys(conf_file()) -> [string()].
keys(#conf_file{lines = Lines}) ->
    [K || {setting, K, _} <- Lines].

-spec rename_keys(conf_file(), [{string(), string()}]) -> conf_file().
rename_keys(CF, []) -> CF;
rename_keys(CF, [{Old, New} | Rest]) ->
    rename_keys(rename_key(CF, Old, New), Rest).

-spec remove_keys(conf_file(), [string()]) -> conf_file().
remove_keys(CF, []) -> CF;
remove_keys(CF, [Key | Rest]) ->
    remove_keys(remove(CF, Key), Rest).

%% ===================================================================
%% Migration Helpers
%% ===================================================================

-spec add_comment_before(conf_file(), string(), string()) -> conf_file().
add_comment_before(#conf_file{lines = Lines} = CF, Key, Text) ->
    CF#conf_file{lines = insert_comment_before(Lines, Key, "# " ++ Text)}.

-spec comment_out(conf_file(), string()) -> conf_file().
comment_out(#conf_file{lines = Lines} = CF, Key) ->
    CF#conf_file{lines = do_comment_out(Lines, Key)}.

%% ===================================================================
%% Internal: Parsing
%% ===================================================================

strip_utf8_bom([16#FEFF | Rest]) -> Rest;
strip_utf8_bom(Other) -> Other.

-spec parse_lines([string()], [line()], normal | {multiline, string(), [string()]}) ->
    {[line()], normal | {multiline, string(), [string()]}}.
parse_lines([], Acc, normal) ->
    {Acc, normal};
parse_lines([], Acc, {multiline, Key, ValueLines}) ->
    Value = join_multiline(ValueLines),
    {[{setting, Key, Value} | Acc], normal};
parse_lines([Line | Rest], Acc, {multiline, Key, ValueLines}) ->
    Stripped = string:trim(strip_cr(Line)),
    case Stripped of
        "'''" ->
            Value = join_multiline(ValueLines),
            parse_lines(Rest, [{setting, Key, Value} | Acc], normal);
        _ ->
            parse_lines(Rest, Acc, {multiline, Key, [strip_cr(Line) | ValueLines]})
    end;
parse_lines([Line | Rest], Acc, normal) ->
    Cleaned = strip_cr(Line),
    Trimmed = string:trim(Cleaned),
    case Trimmed of
        "" ->
            parse_lines(Rest, [empty | Acc], normal);
        [$# | _] ->
            parse_lines(Rest, [{comment, Cleaned} | Acc], normal);
        "include " ++ Path ->
            parse_lines(Rest, [{include, string:trim(Path)} | Acc], normal);
        _ ->
            case parse_setting_line(Cleaned) of
                {multiline_start, Key} ->
                    parse_lines(Rest, Acc, {multiline, Key, []});
                {setting, _, _} = Setting ->
                    parse_lines(Rest, [Setting | Acc], normal);
                false ->
                    parse_lines(Rest, [{comment, Cleaned} | Acc], normal)
            end
    end.

parse_setting_line(Line) ->
    case string:split(Line, "=") of
        [KeyPart, ValuePart] ->
            Key = string:trim(KeyPart),
            case Key of
                "" -> false;
                _  ->
                    RawValue = string:trim(ValuePart),
                    case RawValue of
                        "'''" ->
                            {multiline_start, Key};
                        _ ->
                            {setting, Key, clean_value(RawValue)}
                    end
            end;
        _ ->
            false
    end.

clean_value([$' | Rest]) ->
    case lists:reverse(Rest) of
        [$' | Inner] -> lists:reverse(Inner);
        _            -> [$' | Rest]
    end;
clean_value(Value) ->
    string:trim(strip_inline_comment(Value)).

strip_inline_comment([]) -> [];
strip_inline_comment([$# | _]) -> [];
strip_inline_comment([C | Rest]) -> [C | strip_inline_comment(Rest)].

strip_cr(S) ->
    string:trim(S, trailing, "\r").

join_multiline(ReversedLines) ->
    lists:flatten(lists:join("\n", lists:reverse(ReversedLines))).

%% ===================================================================
%% Internal: Serialization
%% ===================================================================

serialize_line({setting, Key, Value}) ->
    case lists:member($\n, Value) of
        true  -> [Key, " = '''\n", Value, "\n'''"];
        false ->
            case lists:member($#, Value) of
                true  -> [Key, " = '", Value, "'"];
                false -> [Key, " = ", Value]
            end
    end;
serialize_line({comment, Text}) ->
    Text;
serialize_line({include, Path}) ->
    ["include ", Path];
serialize_line(empty) ->
    "".

%% ===================================================================
%% Internal: Operations
%% ===================================================================

find_setting([], _Key) -> undefined;
find_setting([{setting, Key, Value} | _], Key) -> {ok, Value};
find_setting([_ | Rest], Key) -> find_setting(Rest, Key).

has_key([], _) -> false;
has_key([{setting, Key, _} | _], Key) -> true;
has_key([_ | Rest], Key) -> has_key(Rest, Key).

replace_value([], _Key, _Value) -> [];
replace_value([{setting, Key, _} | Rest], Key, Value) ->
    [{setting, Key, Value} | replace_value(Rest, Key, Value)];
replace_value([Other | Rest], Key, Value) ->
    [Other | replace_value(Rest, Key, Value)].

is_setting_for({setting, Key, _}, Key) -> true;
is_setting_for(_, _) -> false.

do_rename([], _Old, _New) -> [];
do_rename([{setting, Old, V} | Rest], Old, New) ->
    [{setting, New, V} | do_rename(Rest, Old, New)];
do_rename([Other | Rest], Old, New) ->
    [Other | do_rename(Rest, Old, New)].

insert_comment_before([], _Key, _Comment) -> [];
insert_comment_before([{setting, Key, _} = S | Rest], Key, Comment) ->
    [{comment, Comment}, S | Rest];
insert_comment_before([Other | Rest], Key, Comment) ->
    [Other | insert_comment_before(Rest, Key, Comment)].

do_comment_out([], _Key) -> [];
do_comment_out([{setting, Key, Value} | Rest], Key) ->
    [{comment, "# " ++ Key ++ " = " ++ Value} | do_comment_out(Rest, Key)];
do_comment_out([Other | Rest], Key) ->
    [Other | do_comment_out(Rest, Key)].

%% ===================================================================
%% Tests
%% ===================================================================

-ifdef(TEST).

parse_empty_test() ->
    CF = parse(""),
    ?assertEqual([], keys(CF)),
    ?assertEqual("", to_string(CF)).

parse_single_setting_test() ->
    CF = parse("key = value\n"),
    ?assertEqual(["key"], keys(CF)),
    ?assertEqual({ok, "value"}, get(CF, "key")).

parse_comment_test() ->
    CF = parse("# this is a comment\n"),
    ?assertEqual([], keys(CF)),
    ?assertEqual("# this is a comment\n", to_string(CF)).

parse_mixed_test() ->
    Input = "# Header\n\nring_size = 32\nlog.file = /var/log/app.log\n",
    CF = parse(Input),
    ?assertEqual(["ring_size", "log.file"], keys(CF)),
    ?assertEqual({ok, "32"}, get(CF, "ring_size")),
    ?assertEqual({ok, "/var/log/app.log"}, get(CF, "log.file")),
    ?assertEqual(Input, to_string(CF)).

parse_inline_comment_test() ->
    CF = parse("ring_size = 32 # smaller ring\n"),
    ?assertEqual({ok, "32"}, get(CF, "ring_size")),
    ?assertEqual("ring_size = 32\n", to_string(CF)).

parse_quoted_value_test() ->
    CF = parse("key = 'value with # hash'\n"),
    ?assertEqual({ok, "value with # hash"}, get(CF, "key")),
    ?assertEqual("key = 'value with # hash'\n", to_string(CF)).

parse_include_test() ->
    CF = parse("include /etc/extra.conf\n"),
    ?assertEqual([], keys(CF)),
    ?assertEqual("include /etc/extra.conf\n", to_string(CF)).

parse_multiline_test() ->
    Input = "a = '''\n    l0\n    l1\n'''\n",
    CF = parse(Input),
    ?assertEqual({ok, "    l0\n    l1"}, get(CF, "a")),
    ?assertEqual(Input, to_string(CF)).

parse_empty_value_test() ->
    CF = parse("key =\n"),
    ?assertEqual({ok, ""}, get(CF, "key")).

parse_value_with_equals_test() ->
    CF = parse("key = a=b\n"),
    ?assertEqual({ok, "a=b"}, get(CF, "key")).

parse_windows_line_endings_test() ->
    CF = parse("key = value\r\n"),
    ?assertEqual({ok, "value"}, get(CF, "key")).

parse_bom_test() ->
    CF = parse([16#FEFF | "key = value\n"]),
    ?assertEqual({ok, "value"}, get(CF, "key")).

roundtrip_test() ->
    Input = "# Config file\n\nlistener.tcp.default = 5672\nlog.level = info\n",
    ?assertEqual(Input, to_string(parse(Input))).

roundtrip_multiline_test() ->
    Input = "cert = '''\n-----BEGIN CERTIFICATE-----\nABC\n-----END CERTIFICATE-----\n'''\n",
    ?assertEqual(Input, to_string(parse(Input))).

roundtrip_include_test() ->
    Input = "include /etc/extra.conf\nkey = val\n",
    ?assertEqual(Input, to_string(parse(Input))).

get_undefined_test() ->
    CF = parse("a = 1\n"),
    ?assertEqual(undefined, get(CF, "b")).

set_existing_test() ->
    CF = parse("key = old\n"),
    CF2 = set(CF, "key", "new"),
    ?assertEqual({ok, "new"}, get(CF2, "key")),
    ?assertEqual("key = new\n", to_string(CF2)).

set_new_key_test() ->
    CF = parse("a = 1\n"),
    CF2 = set(CF, "b", "2"),
    ?assertEqual({ok, "2"}, get(CF2, "b")),
    ?assertEqual(["a", "b"], keys(CF2)).

set_preserves_position_test() ->
    CF = parse("# comment\na = 1\nb = 2\n"),
    CF2 = set(CF, "a", "99"),
    ?assertEqual("# comment\na = 99\nb = 2\n", to_string(CF2)).

remove_test() ->
    CF = parse("a = 1\nb = 2\nc = 3\n"),
    CF2 = remove(CF, "b"),
    ?assertEqual(["a", "c"], keys(CF2)),
    ?assertEqual("a = 1\nc = 3\n", to_string(CF2)).

remove_nonexistent_test() ->
    CF = parse("a = 1\n"),
    CF2 = remove(CF, "b"),
    ?assertEqual(["a"], keys(CF2)).

rename_key_test() ->
    CF = parse("# Settings\nold.key = value\nother = x\n"),
    CF2 = rename_key(CF, "old.key", "new.key"),
    ?assertEqual(undefined, get(CF2, "old.key")),
    ?assertEqual({ok, "value"}, get(CF2, "new.key")),
    ?assertEqual("# Settings\nnew.key = value\nother = x\n", to_string(CF2)).

rename_keys_test() ->
    CF = parse("a = 1\nb = 2\n"),
    CF2 = rename_keys(CF, [{"a", "x"}, {"b", "y"}]),
    ?assertEqual(["x", "y"], keys(CF2)).

remove_keys_test() ->
    CF = parse("a = 1\nb = 2\nc = 3\n"),
    CF2 = remove_keys(CF, ["a", "c"]),
    ?assertEqual(["b"], keys(CF2)).

add_comment_before_test() ->
    CF = parse("a = 1\nb = 2\n"),
    CF2 = add_comment_before(CF, "b", "Migrated"),
    ?assertEqual("a = 1\n# Migrated\nb = 2\n", to_string(CF2)).

comment_out_test() ->
    CF = parse("a = 1\nb = 2\nc = 3\n"),
    CF2 = comment_out(CF, "b"),
    ?assertEqual(undefined, get(CF2, "b")),
    ?assertEqual("a = 1\n# b = 2\nc = 3\n", to_string(CF2)).

comment_out_preserves_others_test() ->
    CF = parse("a = 1\nb = 2\n"),
    CF2 = comment_out(CF, "b"),
    ?assertEqual({ok, "1"}, get(CF2, "a")).

keys_empty_test() ->
    ?assertEqual([], keys(parse(""))).

keys_order_test() ->
    CF = parse("z = 1\na = 2\nm = 3\n"),
    ?assertEqual(["z", "a", "m"], keys(CF)).

multiline_unterminated_test() ->
    CF = parse("key = '''\nsome content"),
    ?assertEqual({ok, "some content"}, get(CF, "key")).

setting_with_hash_serialization_test() ->
    CF = set(parse(""), "key", "value#1"),
    ?assertEqual("key = 'value#1'", to_string(CF)).

multiline_serialization_test() ->
    CF = set(parse(""), "key", "line1\nline2"),
    ?assertEqual("key = '''\nline1\nline2\n'''", to_string(CF)).

setting_with_hash_roundtrip_test() ->
    Input = "key = 'has # hash'\n",
    ?assertEqual(Input, to_string(parse(Input))).

multiline_value_via_set_test() ->
    CF = set(parse("other = 1\n"), "key", "l1\nl2"),
    ?assertEqual({ok, "l1\nl2"}, get(CF, "key")).

load_nonexistent_test() ->
    ?assertMatch({error, enoent}, load("/nonexistent/path/file.conf")).

duplicate_key_rename_test() ->
    CF = parse("key = 1\nkey = 2\n"),
    CF2 = rename_key(CF, "key", "new"),
    ?assertEqual(["new", "new"], keys(CF2)).

-endif.
