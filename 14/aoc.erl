-module(aoc).
-export([part1/0]).
-export([part2/0]).

part1() ->
    Grid = input(),
    MaxY = lists:max(lists:map(fun ({_, Yy}) -> Yy end, maps:keys(Grid))),
    GetPos = fun
                 (_, {_, Y} = Pos) when Y > MaxY -> {abyss, Pos};
                 (G, Pos) -> {maps:get(Pos, G, empty), Pos}
             end,
    pour(Grid, 0, GetPos).

part2() ->
    Grid = input(),
    MaxY = lists:max(lists:map(fun ({_, Yy}) -> Yy end, maps:keys(Grid))) + 2,
    GetPos = fun
                 (_, {_, Y} = Pos) when Y == MaxY -> {rock, Pos};
                 (G, Pos) -> {maps:get(Pos, G, empty), Pos}
             end,
    pour(Grid, 0, GetPos).

pour(Grid, Count, GetPosFn) ->
    case sand(Grid, {500, 0}, fall, GetPosFn) of
        abyss -> Count; % part 1 stop condition
        NewGrid when Grid == NewGrid -> Count; % part 2 stop condition
        NewGrid -> pour(NewGrid, Count + 1, GetPosFn)
    end.

sand(Grid, Pos, fall, GetPosFn) ->
    case fall(Grid, Pos, GetPosFn) of
        {empty, Down} -> sand(Grid, Down, fall, GetPosFn);
        {abyss, _} -> abyss;
        {sand, _} -> sand(Grid, Pos, left, GetPosFn);
        {rock, _} -> sand(Grid, Pos, left, GetPosFn)
    end;
sand(Grid, Pos, left, GetPosFn) ->
    case left(Grid, Pos, GetPosFn) of
        {empty, Left} -> sand(Grid, Left, fall, GetPosFn);
        {abyss, _} -> abyss;
        {sand, _} -> sand(Grid, Pos, right, GetPosFn);
        {rock, _} -> sand(Grid, Pos, right, GetPosFn)
    end;
sand(Grid, Pos, right, GetPosFn) ->
    case right(Grid, Pos, GetPosFn) of
        {empty, Right} -> sand(Grid, Right, fall, GetPosFn);
        {abyss, _} -> abyss;
        {sand, _} -> maps:put(Pos, sand, Grid);
        {rock, _} -> maps:put(Pos, sand, Grid)
    end.

fall(Grid, {X, Y}, GetPosFn) ->
    GetPosFn(Grid, {X, Y + 1}).
left(Grid, {X, Y}, GetPosFn) ->
    GetPosFn(Grid, {X - 1, Y + 1}).
right(Grid, {X, Y}, GetPosFn) ->
    GetPosFn(Grid, {X + 1, Y + 1}).

input() ->
    {ok, I} = file:read_file(input),
    lists:foldl(fun parse_path/2, #{}, binary:split(I, <<"\n">>, [global])).

parse_path(Path, Grid) ->
    [Start | Coords] = to_coords(Path),
    {_, G} = lists:foldl(fun (Til, {From, Agg}) ->
                             NewAgg = lists:foldl(fun (C, A) ->
                                                     maps:put(C, rock, A)
                                                 end, Agg, between(From, Til)),
                             {Til, NewAgg}
                         end, {Start, Grid}, Coords),
    G.

to_coords(Path) ->
    lists:map(fun (C) ->
                  [X, Y] = binary:split(C, <<",">>),
                  {binary_to_integer(X), binary_to_integer(Y)}
              end, binary:split(Path, <<" -> ">>, [global])).

between({FromX, Y}, {ToX, Y}) ->
    [Start, Finish] = lists:sort([FromX, ToX]),
    lists:map(fun (X) -> {X, Y} end, lists:seq(Start, Finish));
between({X, FromY}, {X, ToY}) ->
    [Start, Finish] = lists:sort([FromY, ToY]),
    lists:map(fun (Y) -> {X, Y} end, lists:seq(Start, Finish)).
