-module(aoc).
-export([part1/0]).
-export([part2/0]).

part1() ->
    monkey_business_level(round(input(), 0, 1, 20, fun worry_level_1/2)).

part2() ->
    Input = input(),
    monkey_business_level(round(Input, 0, 1, 10000, worry_level_2(Input))).

round(State, Monkey, Round, LastRound, WorryLevelFn) ->
    case maps:get(Monkey, State, no_monkey) of
        no_monkey when Round == LastRound ->
            State;
        no_monkey ->
            round(State, 0, Round + 1, LastRound, WorryLevelFn);
        #{counter := Counter, items := Items, op := Op, test := Test} = MonkeyState ->
            NewItems = lists:map(fun (Item) -> WorryLevelFn(Item, Op) end, Items),
            ToThrow = throw_test(NewItems, Test),
            NewState = lists:foldl(fun ({M, I}, Acc) ->
                                    #{items := PrevItems} = Prev = maps:get(M, Acc),
                                    Acc#{M := Prev#{items := PrevItems ++ I}}
                                end, State, maps:to_list(ToThrow)),
            NewMonkeyState = MonkeyState#{counter := Counter + length(Items), items := []},
            round(NewState#{Monkey := NewMonkeyState}, Monkey + 1, Round, LastRound, WorryLevelFn)
    end.

worry_level_1(Item, {Op, <<"old">>}) -> op(Item, Op, Item) div 3;
worry_level_1(Item, {Op, V}) -> op(Item, Op, V) div 3.

worry_level_2(Input) ->
    Divs = lists:map(fun (#{test := #{v := V}}) -> V end, maps:values(Input)),
    Mod = lists:foldl(fun (V, C) -> V * C end, 1, Divs),
    fun (Item, {Op, <<"old">>}) -> op(Item, Op, Item) rem Mod;
        (Item, {Op, V}) -> op(Item, Op, V) rem Mod
    end.

op(A, <<"+">>, B) -> A + B;
op(A, <<"*">>, B) -> A * B.

throw_test(Items, Test) -> throw_test(Items, Test, #{}).

throw_test([], _, Acc) ->
    Acc;
throw_test([H | T], #{v := V, true := True} = Test, Acc) when H rem V == 0 ->
    NewAcc = maps:update_with(True, fun (Items) -> Items ++ [H] end, [H], Acc),
    throw_test(T, Test, NewAcc);
throw_test([H | T], #{false := False} = Test, Acc) ->
    NewAcc = maps:update_with(False, fun (Items) -> Items ++ [H] end, [H], Acc),
    throw_test(T, Test, NewAcc).

monkey_business_level(Monkeys) ->
    Counters = ordsets:from_list(lists:map(fun (#{counter := C}) -> C end, maps:values(Monkeys))),
    [A, B | _] = lists:reverse(Counters),
    A * B.

input() ->
    {ok, I} = file:read_file(input),
    {Acc, _} = lists:foldl(fun parse_line/2, {#{}, unknown}, binary:split(I, <<"\n">>, [global])),
    Acc.

parse_line(<<"Monkey ", M/binary>>, {Acc, _}) ->
    Monkey = binary_to_integer(binary:replace(M, <<":">>, <<"">>)),
    {Acc#{Monkey => #{counter => 0}}, Monkey};
parse_line(<<>>, Acc) ->
    Acc;
parse_line(<<"  Starting items: ", I/binary>>, {Acc, Current}) ->
    Items = lists:map(fun binary_to_integer/1, binary:split(I, <<", ">>, [global])),
    {Acc#{Current => maps:put(items, Items, maps:get(Current, Acc))}, Current};
parse_line(<<"  Operation: new = ", O/binary>>, {Acc, Current}) ->
    case binary:split(O, <<" ">>, [global]) of
        [<<"old">>, Op, <<"old">>] ->
            {Acc#{Current => maps:put(op, {Op, <<"old">>}, maps:get(Current, Acc))}, Current};
        [<<"old">>, Op, V] ->
            {Acc#{Current => maps:put(op, {Op, binary_to_integer(V)}, maps:get(Current, Acc))}, Current}
    end;
parse_line(<<"  Test: divisible by ", T/binary>>, {Acc, Current}) ->
    {Acc#{Current => maps:put(test, #{v => binary_to_integer(T)}, maps:get(Current, Acc))}, Current};
parse_line(<<"    If true: throw to monkey ", M/binary>>, {Acc, Current}) ->
    #{test := PrevTest} = Prev = maps:get(Current, Acc),
    NewTest = PrevTest#{true => binary_to_integer(M)},
    {Acc#{Current => Prev#{test := NewTest}}, Current};
parse_line(<<"    If false: throw to monkey ", M/binary>>, {Acc, Current}) ->
    #{test := PrevTest} = Prev = maps:get(Current, Acc),
    NewTest = PrevTest#{false => binary_to_integer(M)},
    {Acc#{Current => Prev#{test := NewTest}}, Current}.