-module(aoc).
-export([part1/0]).
-export([part2/0]).

part1() ->
    by_half(input(), 0).

part2() ->
    by_three(input(), 0).

by_half([], Sum) -> Sum;
by_half([H | T], Sum) ->
    Half = byte_size(H) div 2,
    <<First:Half/binary, Second:Half/binary>> = H,
    P = lists:sum(lists:map(fun priority/1, intersection([First, Second]))),
    by_half(T, P + Sum).

by_three([], Sum) -> Sum;
by_three([F, S, T | R], Sum) ->
    P = lists:sum(lists:map(fun priority/1, intersection([F, S, T]))),
    by_three(R, P + Sum).

intersection(Binaries) ->
    Ordsets = lists:map(fun (B) -> ordsets:from_list(binary_to_list(B)) end, Binaries),
    ordsets:intersection(Ordsets).

priority(C) when C < $a -> C - 38;
priority(C) -> C - 96.

input() ->
    {ok, I} = file:read_file(input),
    binary:split(I, <<"\n">>, [global]).