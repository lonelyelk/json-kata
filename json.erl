-module(json).

-export([parse/1, parse_test/0, string_test/0]).

parse(L) when is_list(L) ->
    case string:trim(L, both, [16#20, $\t, $\n, $\r]) of
        [${ | Tail] ->
            case parse_object_key_or_close(Tail, []) of
                {_, _} -> error(bad_json);
                Res -> Res
            end;
        [$[ | Tail] ->
            case parse_array_value_or_close(Tail, []) of
                {_, _} -> error(bad_json);
                Res -> Res
            end;
        _ ->
            error(bad_json)
    end.

parse_object_key_or_close(L, Acc) ->
    case string:trim(L, both, [16#20, $\t, $\n, $\r]) of
        [$} | []] ->
            lists:reverse(Acc);
        [$} | Tail] ->
            {Tail, lists:reverse(Acc)};
        _ ->
            parse_object_key_only(L, Acc)
    end.

parse_object_comma_or_close(L, Acc) ->
    case string:trim(L, both, [16#20, $\t, $\n, $\r]) of
        [$} | []] ->
            lists:reverse(Acc);
        [$} | Tail] ->
            {Tail, lists:reverse(Acc)};
        [$, | Tail] when length(Acc) > 0 ->
            parse_object_key_only(Tail, Acc);
        _ ->
            error(bad_json)
    end.

parse_object_key_only(L, Acc) ->
    case string:trim(L, both, [16#20, $\t, $\n, $\r]) of
        [$" | Tail] -> % "
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
    case string:trim(L, both, [16#20, $\t, $\n, $\r]) of
        [$" | Tail] ->
            {Value, NewTail} = parse_string(Tail, ""),
            parse_object_comma_or_close(NewTail, [{Key, Value} | Acc]);
        [${ | Tail] ->
            case parse_object_key_or_close(Tail, []) of
                {NewTail, Value} ->
                    parse_object_comma_or_close(NewTail, [{Key, Value} | Acc]);
                _ ->
                    error(bad_json)
            end;
        [$[ | Tail] ->
            case parse_array_value_or_close(Tail, []) of
                {NewTail, Value} ->
                    parse_object_comma_or_close(NewTail, [{Key, Value} | Acc]);
                _ ->
                    error(bad_json)
            end;
        [$t, $r, $u, $e | Tail] ->
            parse_object_comma_or_close(Tail, [{Key, true} | Acc]);
        [$f, $a, $l, $s, $e | Tail] ->
            parse_object_comma_or_close(Tail, [{Key, false} | Acc]);
        [$n, $u, $l, $l | Tail] ->
            parse_object_comma_or_close(Tail, [{Key, null} | Acc]);
        _ ->
            error(bad_json)
    end.

parse_array_value_or_close(L, Acc) ->
    case string:trim(L, both, [16#20, $\t, $\n, $\r]) of
        [$" | Tail] ->
            {Value, NewTail} = parse_string(Tail, ""),
            parse_array_comma_or_close(NewTail, [Value | Acc]);
        [$[ | Tail] ->
            case parse_array_value_or_close(Tail, []) of
                {NewTail, Arr} ->
                    parse_array_comma_or_close(NewTail, [Arr | Acc]);
                _ ->
                    error(bad_json)
            end;
        [$] | []] ->
            lists:reverse(Acc);
        [$] | Tail] ->
            {Tail, lists:reverse(Acc)};
        [${ | Tail] ->
            case parse_object_key_or_close(Tail, []) of
                {NewTail, Obj} ->
                    parse_array_comma_or_close(NewTail, [Obj | Acc]);
                _ ->
                    error(bad_json)
            end;
        [$t, $r, $u, $e | Tail] ->
            parse_array_comma_or_close(Tail, [true | Acc]);
        [$f, $a, $l, $s, $e | Tail] ->
            parse_array_comma_or_close(Tail, [false | Acc]);
        [$n, $u, $l, $l | Tail] ->
            parse_array_comma_or_close(Tail, [null | Acc]);
        _ ->
            error(bad_json)
    end.

parse_array_value_only(L, Acc) ->
    case string:trim(L, both, [16#20, $\t, $\n, $\r]) of
        [$" | Tail] ->
            {Value, NewTail} = parse_string(Tail, ""),
            parse_array_comma_or_close(NewTail, [Value | Acc]);
        [$[ | Tail] ->
            case parse_array_value_or_close(Tail, []) of
                {NewTail, Arr} ->
                    parse_array_comma_or_close(NewTail, [Arr | Acc]);
                _ ->
                    error(bad_json)
            end;
        [${ | Tail] ->
            case parse_object_key_or_close(Tail, []) of
                {NewTail, Obj} ->
                    parse_array_comma_or_close(NewTail, [Obj | Acc]);
                _ ->
                    error(bad_json)
            end;
        [$t, $r, $u, $e | Tail] ->
            parse_array_comma_or_close(Tail, [true | Acc]);
        [$f, $a, $l, $s, $e | Tail] ->
            parse_array_comma_or_close(Tail, [false | Acc]);
        [$n, $u, $l, $l | Tail] ->
            parse_array_comma_or_close(Tail, [null | Acc]);
        _ ->
            error(bad_json)
    end.

parse_array_comma_or_close(L, Acc) ->
    case string:trim(L, both, [16#20, $\t, $\n, $\r]) of
        [$, | Tail] ->
            parse_array_value_only(Tail, Acc);
        [$] | []] ->
            lists:reverse(Acc);
        [$] | Tail] ->
            {Tail, lists:reverse(Acc)};
        _ ->
            error(bad_json)
    end.

parse_string(L, Str) ->
    case L of
        "" ->
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
    [true, false, null] = parse("[true, false, null]")
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