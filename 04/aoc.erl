-module(aoc).
-export([part1/0]).
-export([part2/0]).
-export([has_overlap/1]).

part1() ->
    length(lists:filter(fun has_full_overlap/1, input())).

part2() ->
    length(lists:filter(fun has_overlap/1, input())).

has_full_overlap([A, B, C, D]) when (A >= C) andalso (B =< D) -> true;
has_full_overlap([A, B, C, D]) when (C >= A) andalso (D =< B) -> true;
has_full_overlap(_) -> false.

has_overlap([A, B, C, D]) when (A > D) orelse (C > B) -> false;
has_overlap(_) -> true.

input() ->
    {ok, I} = file:read_file(input),
    lists:map(fun (Line) ->
                  S = binary:split(Line, [<<",">>, <<"-">>], [global]),
                  lists:map(fun binary_to_integer/1, S)
              end, binary:split(I, <<"\n">>, [global])).