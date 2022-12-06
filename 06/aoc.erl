-module(aoc).
-export([part1/0]).
-export([part2/0]).

part1() ->
    run(input(), 4).

part2() ->
    run(input(), 14).

run(Input, Length) ->
    List = binary:bin_to_list(Input),
    {H, T} = lists:split(Length, List),
    check(T, Length, Length, H).

check([Hi | Ti], Length, Index, [_ | Tc] = Chars) ->
    case length(ordsets:from_list(Chars)) of
        Length -> Index;
        _ -> check(Ti, Length, Index + 1, Tc ++ [Hi])
    end.

input() ->
    {ok, I} = file:read_file(input),
    I.