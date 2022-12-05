-module(aoc).
-export([part1/0]).
-export([part2/0]).

part1() ->
    run(part1).

part2() ->
    run(part2).

run(Part) ->
    Stacks = process(input(), Part),
    OrderedStacks = lists:sort(fun ({I1, _}, {I2, _}) -> I2 > I1 end, maps:to_list(Stacks)),
    lists:foldl(fun ({_, Stack}, Acc) ->
                    erlang:iolist_to_binary([Acc] ++ [hd(Stack)]) end,
                <<>>, OrderedStacks).

process({Stacks, []}, _) ->
    Stacks;
process({Stacks, [Move | Rest]}, Part) ->
    NewStacks = move(Stacks, Move, Part),
    process({NewStacks, Rest}, Part).

move(Stacks, #{amount := Amount, from := From, to := To}, Part) ->
    {Taking, Remaining} = lists:split(Amount, maps:get(From, Stacks)),
    Crates = case Part of
                 part1 -> lists:reverse(Taking);
                 part2 -> Taking
             end,
    Stacks#{To := Crates ++ maps:get(To, Stacks), From := Remaining}.

input() ->
    {ok, I} = file:read_file(input),
    lists:foldl(fun
                    (<<"[", _/binary>> = Line, Acc) ->
                        stacks(Line, Acc, 1);
                    (<<" 1", _/binary>>, {Stacks, Moves}) ->
                        ReversedStacks = lists:foldl(fun ({K, V}, Acc) ->
                                                         Acc#{K => lists:reverse(V)}
                                                     end, #{}, maps:to_list(Stacks)),
                        {ReversedStacks, Moves};
                    (<<"move", _/binary>> = Line, Acc) ->
                        parse_move(Line, Acc);
                    (eof, {Stacks, Moves}) ->
                        {Stacks, lists:reverse(Moves)};
                    (_, Acc) ->
                        Acc
              end, {#{}, []}, binary:split(I, <<"\n">>, [global]) ++ [eof]).

stacks(<<Current:3/binary, Rest/binary>>, {Stacks, Moves}, I) ->
    NewStacks = maps:update_with(I,
                                 fun (S) -> add_crate(Current, S) end,
                                 add_crate(Current, []),
                                 Stacks),
    case Rest of
        <<>> -> {NewStacks, Moves};
        <<" ", Remaining/binary>> -> stacks(Remaining, {NewStacks, Moves}, I + 1)
    end.

add_crate(<<"   ">>, Stack) -> Stack;
add_crate(<<"[", Crate:1/binary, "]">>, Stack) -> [Crate | Stack].

parse_move(Line, {Stacks, Moves}) ->
    [_, Amount, _, From, _, To] = binary:split(Line, <<" ">>, [global]),
    Move = #{amount => binary_to_integer(Amount),
             from => binary_to_integer(From),
             to => binary_to_integer(To)},
    {Stacks, [Move | Moves]}.