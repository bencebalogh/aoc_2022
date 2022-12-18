-module(aoc).
-export([part1/0]).
-export([part2/0]).

-define(PART1_Y, 2000000).
-define(MAX, 4000000).

part1() ->
    {Empty, Beacon} = from_sensors(input(),
                                   ?PART1_Y,
                                   ordsets:new(),
                                   ordsets:new(),
                                   fun min_x_1/2,
                                   fun max_x_1/2),
    L1 = lists:foldl(fun ({X1, X2}, A) ->
                    ordsets:union(ordsets:from_list(lists:seq(X1, X2)), A)
                end, ordsets:new(), Empty),
    length(L1) - length(Beacon).

part2() ->
    find(0, input()).

find(Y, Input) ->
    Known = from_sensors(Input, Y, ordsets:new(), ordsets:new(), fun min_x_2/2, fun max_x_2/2),
    case check(Y, Known) of
        not_solution ->
            find(Y + 1, Input);
        Solution ->
            tuning_frequency(Solution)
    end.

check(Y, {Empty, Beacon}) ->
    F = lists:foldl(fun
                        ({Xs, _}, C) when C + 2 == Xs -> {found, C + 1};
                        ({Xs, Xe}, C) when Xs == C -> Xe;
                        ({Xs, Xe}, C) when (Xs < C) andalso (Xe > C) -> Xe;
                        ({Xs, _}, C) when Xs < C -> C;
                        ({Xs, Xe}, C) when Xs == C -> Xe
                    end, 0, Empty),
    case F of
        {found, X} ->
            case ordsets:is_element(X, Beacon) of
                false -> {X, Y};
                true -> not_solution
            end;
        _ -> not_solution
    end.

from_sensors([], _, Empty, Beacon, _, _) ->
    {Empty, Beacon};
from_sensors([{{Sx, Sy} = S, {Bx, By} = B} | Rest], Y, Empty, Beacon, MinXFn, MaxXFn) ->
    Dist = manhattan_distance(S, B),
    ToY = abs(Y - Sy),
    Left = Dist - ToY,
    MinX = MinXFn(Sx, Left),
    MaxX = MaxXFn(Sx, Left),
    case ToY =< Dist of
        true ->
            NewBeacon = case By == Y of
                            true -> ordsets:add_element(Bx, Beacon);
                            false -> Beacon
                        end,
            NewEmpty = ordsets:add_element({MinX, MaxX}, Empty),
            from_sensors(Rest, Y, NewEmpty, NewBeacon, MinXFn, MaxXFn);
        false ->
            from_sensors(Rest, Y, Empty, Beacon, MinXFn, MaxXFn)
    end.

manhattan_distance({X1, Y1}, {X2, Y2}) -> abs(X1 - X2) + abs(Y1 - Y2).

tuning_frequency({X, Y}) -> X * 4000000 + Y.

min_x_1(X, Left) -> X - Left.

max_x_1(X, Left) -> X + Left.

min_x_2(X, Left) when X - Left >= 0 -> X - Left;
min_x_2(_, _) -> 0.

max_x_2(X, Left) when X + Left =< ?MAX -> X + Left;
max_x_2(_, _) -> ?MAX.

input() ->
    {ok, I} = file:read_file(input),
    lists:foldl(fun parse_line/2, [], binary:split(I, <<"\n">>, [global])).

parse_line(<<"Sensor at x=", Line/binary>>, Agg) ->
     P = binary:split(Line, [<<", y=">>, <<": closest beacon is at x=">>], [global]),
     [Sx, Sy, Bx, By] = lists:map(fun binary_to_integer/1, P),
     [{{Sx, Sy}, {Bx, By}} | Agg].
