-module(aoc).
-export([part1/0]).
-export([part2/0]).

part1() ->
    run([0]).

part2() ->
    run([-2, -1, 0]).

run(Init) ->
    {_, Ms} = lists:foldl(fun
        (<<>>, {C, [M | Mr]}) when C > M -> {0, ordsets:from_list([C | Mr])};
        (<<>>, {_, M}) -> {0, M};
        (I, {C, M}) -> {binary_to_integer(I) + C, M}
    end, {0, ordsets:from_list(Init)}, input()),
    lists:sum(Ms).

input() ->
    {ok, I} = file:read_file(input),
    binary:split(I, <<"\n">>, [global]).
