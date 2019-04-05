-module(json).

-export([parse/1, parse_test/0, string_test/0]).

parse(L) when is_list(L) ->
    case parse_value(L) of
        {Res, []} ->
            Res;
        _ ->
            error(bad_json)
    end.

parse_object_key_or_close(L, Acc) ->
    case string:trim(L, both, [16#20, $\t, $\n, $\r]) of
        [$} | Tail] ->
            {lists:reverse(Acc), Tail};
        _ ->
            parse_object_key_only(L, Acc)
    end.

parse_object_comma_or_close(L, Acc) ->
    case string:trim(L, both, [16#20, $\t, $\n, $\r]) of
        [$} | Tail] ->
            {lists:reverse(Acc), Tail};
        [$, | Tail] ->
            parse_object_key_only(Tail, Acc);
        _ ->
            error(bad_json)
    end.

parse_object_key_only(L, Acc) ->
    case string:trim(L, both, [16#20, $\t, $\n, $\r]) of
        [$" | Tail] ->
            {Key, NewTail} = parse_string(Tail, ""),
            parse_object_separator(NewTail, Key, Acc);
        _ ->
            error(bad_json)
    end.

parse_object_separator(L, Key, Acc) ->
    case string:trim(L, both, [16#20, $\t, $\n, $\r]) of
        [$: | Tail] ->
            parse_object_value(Tail, Key, Acc);
        _ ->
            error(bad_json)
    end.

parse_object_value(L, Key, Acc) ->
    {Value, NewTail} = parse_value(L),
    parse_object_comma_or_close(NewTail, [{Key, Value} | Acc]).

parse_array_value_or_close(L, Acc) ->
    case string:trim(L, both, [16#20, $\t, $\n, $\r]) of
        [$] | Tail] ->
            {lists:reverse(Acc), Tail};
        _ ->
            parse_array_value_only(L, Acc)
    end.

parse_array_value_only(L, Acc) ->
    {Value, NewTail} = parse_value(L),
    parse_array_comma_or_close(NewTail, [Value | Acc]).

parse_array_comma_or_close(L, Acc) ->
    case string:trim(L, both, [16#20, $\t, $\n, $\r]) of
        [$] | Tail] ->
            {lists:reverse(Acc), Tail};
        [$, | Tail] ->
            parse_array_value_only(Tail, Acc);
        _ ->
            error(bad_json)
    end.

parse_value(L) ->
    case string:trim(L, both, [16#20, $\t, $\n, $\r]) of
        [$" | Tail] ->
            parse_string(Tail, "");
        [$[ | Tail] ->
            parse_array_value_or_close(Tail, []);
        [${ | Tail] ->
            parse_object_key_or_close(Tail, []);
        [$t, $r, $u, $e | Tail] ->
            {true, Tail};
        [$f, $a, $l, $s, $e | Tail] ->
            {false, Tail};
        [$n, $u, $l, $l | Tail] ->
            {null, Tail};
        LTr ->
            parse_number(LTr)
    end.

parse_string(L, Str) ->
    case L of
        [] ->
            error(bad_json);
        [$\\, $b | Tail] -> % backspace
            parse_string(Tail, [$\b | Str]);
        [$\\, $f | Tail] -> % form fee
            parse_string(Tail, [$\f | Str]);
        [$\\, $n | Tail] -> % line fee
            parse_string(Tail, [$\n | Str]);
        [$\\, $r | Tail] -> % carriage return
            parse_string(Tail, [$\r | Str]);
        [$\\, $t | Tail] -> % tab
            parse_string(Tail, [$\t | Str]);
        [$\\, $u, A, B, C, D | Tail] -> % \u0000
            try
                case list_to_integer([A, B, C, D], 16) of
                    SurrogateHigh when SurrogateHigh >= 16#d800, SurrogateHigh =< 16#dbff ->
                        [$\\, $u, E, F, G, H | NewTail] = Tail,
                        case list_to_integer([E, F, G, H], 16) of
                            SurrogateLow when SurrogateLow >= 16#dc00, SurrogateLow =< 16#dfff ->
                                Utf16 = ((SurrogateHigh - 16#d800) bsl 10) + SurrogateLow - 16#dc00 + 16#10000,
                                parse_string(NewTail, [Utf16 | Str]);
                            _ ->
                                error(bad_json)
                        end;
                    Utf8 when Utf8 =< 16#d7ff; Utf8 >= 16#e000 ->
                        parse_string(Tail, [Utf8 | Str]);
                    _ ->
                        error(bad_json)
                end
            catch
                error:{badmatch, _} -> error(bad_json);
                error:badarg -> error(bad_json)
            end;
        [$\\, $u | _] -> % \uX<4
            error(bad_json);
        [$\\, Chr | Tail] when Chr =:= $" ; Chr =:= $\\ ; Chr =:= $/ ->
            parse_string(Tail, [Chr | Str]);
        [$" | Tail] ->
            {lists:reverse(Str), Tail};
        [Chr | Tail] ->
            parse_string(Tail, [Chr | Str])
    end.

parse_number(L) ->
    case re:run(L, "^-?0\\.\\d+(e\\d+)?", [caseless]) of
        {match, [{0, Len}| _]} ->
            parse_float_match(L, Len);
        nomatch ->
            case re:run(L, "^-?[1-9]\\d*(\\.\\d+)?(e[+-]?\\d+)?", [caseless]) of
                {match, [{0, Len}]} ->
                    {list_to_integer(lists:sublist(L, Len)), lists:nthtail(Len, L)};
                {match, [{0, Len}, {-1,0}, {PosE, LenE}]} ->
                    try string:to_float(lists:sublist(L, PosE) ++ ".0" ++ lists:sublist(L, PosE+1, LenE)) of
                        {F, _} ->
                            {F, lists:nthtail(Len, L)};
                        _ ->
                            error(bad_json)
                    catch
                        error:badarg -> error(bad_json)
                    end;
                {match, [{0, Len} | _]} ->
                    parse_float_match(L, Len);
                nomatch ->
                    error(bad_json)
            end
    end.

parse_float_match(L, Len) ->
    try string:to_float(lists:sublist(L, Len)) of
        {F, _} ->
            {F, lists:nthtail(Len, L)};
        _ ->
            error(bad_json)
    catch
        error:badarg -> error(bad_json)
    end.

parse_test() ->
    [] = parse("{}"),
    [] = parse("  {}"),
    [] = parse(" \n{ \t}"),
    [{"key", "value"}] = parse("{\"key\": \"value\"}"),
    [{"key", "value"}, {"key2", "value2"}] = parse("{\"key\": \"value\",\n\"key2\": \"value2\"}"),
    ok = try parse("{\"key\": \"value\",}")
    catch
        error:bad_json -> ok
    end,
    ok = try parse("{\"key\": \"value\",\n\"key2\": \"value2\",}")
    catch
        error:bad_json -> ok
    end,
    ok = try parse("{\"key\": ")
    catch
        error:bad_json -> ok
    end,
    ok = try parse("{\"key\": \"")
    catch
        error:bad_json -> ok
    end,
    ok = try parse("{\"key\": \"\"")
    catch
        error:bad_json -> ok
    end,
    [{"obj", [{"key1", "value1"}, {"obj2", [{"key2", "value2"}]}]}, {"key3", "value3"}] =
        parse("{\"obj\": {\"key1\": \"value1\", \"obj2\": {\"key2\": \"value2\"}}, \"key3\": \"value3\"}"),
    ok = try parse("{\"key\": \"value\"\"key2\": \"value2\"}")
    catch
        error:bad_json -> ok
    end,
    ok = try parse("{,\"key\": \"value\"}")
    catch
        error:bad_json -> ok
    end,
    [{"key", true}] = parse("{\"key\": true}"),
    [{"key", false}] = parse("{\"key\": false}"),
    [{"key", null}] = parse("{\"key\": null}"),
    ok = try parse("{\"key\": \"value\"}{}")
    catch
        error:bad_json -> ok
    end,
    ["value", "value"] = parse("[\"value\", \"value\"]"),
    ["value", []] = parse("[\"value\", []]"),
    ["value", ["a"], "value2"] = parse("[\"value\", [\"a\"], \"value2\"]"),
    ["a", ["b"], [{"key", "value"}], [], "c"] = parse("[\"a\", [\"b\"], {\"key\": \"value\"}, {}, \"c\"]"),
    ok = try parse("[]{}")
    catch
        error:bad_json -> ok
    end,
    ok = try parse("[,]")
    catch
        error:bad_json -> ok
    end,
    ok = try parse("[\"a\",]")
    catch
        error:bad_json -> ok
    end,
    [{"key", ["a", "b", [{"key", "value"}]]}] = parse("{\"key\": [\"a\", \"b\", {\"key\": \"value\"}]}"),
    [true, false, null] = parse("[true, false, null]"),
    [{"a", 10}, {"b", [10, -20]}] = parse("{\"a\": 10, \"b\": [10, -20]}"),
    [{"a", 0.12}, {"b", [10.234, -2.023e201]}] = parse("{\"a\": 0.12, \"b\": [10.234, -20.23E+200]}"),
    0.02 = parse("2e-2"),
    ok = try parse("010")
    catch
        error:bad_json -> ok
    end,
    ok = try parse("0.10.10")
    catch
        error:bad_json -> ok
    end
.

string_test() ->
    {"0a\nb\"c/", "abc"} = parse_string("a\\nb\\\"c\\/\"abc", "0"),
    {"ü", ""} = parse_string("ü\"", ""),
    {"❤", ""} = parse_string("❤\"", ""),
    {"ü", ""} = parse_string("\\u00fc\"", ""),
    {"❤", ""} = parse_string("\\u2764\"", ""),
    ok = try parse_string("\\uvbnm\"", "")
    catch
        error:bad_json -> ok
    end,
    ok = try parse_string("\\uf\"", "")
    catch
        error:bad_json -> ok
    end,
    {"𝄞", ""} = parse_string("\\ud834\\udd1e\"", ""),
    ok = try parse_string("\\ud834\"", "")
    catch
        error:bad_json -> ok
    end,
    ok = try parse_string("\\ud834\\uqqqq\"", "")
    catch
        error:bad_json -> ok
    end,
    ok = try parse_string("\\ud834\\ueeee\"", "")
    catch
        error:bad_json -> ok
    end
.