-module(aoc).
-export([part1/0]).
-export([part2/0]).

part1() ->
    Grid = input(),
    count_visibles(maps:to_list(Grid), Grid, 0).

part2() ->
    Grid = input(),
    max_scenic_score(maps:to_list(Grid), Grid, 0).

count_visibles([], _, Count) ->
    Count;
count_visibles([H | T], Grid, Count) ->
    NewCount = case visible(H, Grid) of
                   true -> Count + 1;
                   false -> Count
               end,
    count_visibles(T, Grid, NewCount).

visible(Tree, Grid) ->
    visible(left, Tree, Grid)
    orelse visible(right, Tree, Grid)
    orelse visible(top, Tree, Grid)
    orelse visible(bottom, Tree, Grid).

visible(Direction, {Pos, Height}, Grid) ->
    NeighbourPos = neighbour(Direction, Pos),
    case maps:get(NeighbourPos, Grid, finished) of
        finished ->
            true;
        Neighbour when Neighbour >= Height ->
            false;
        _ ->
            visible(Direction, {NeighbourPos, Height}, Grid)
    end.

max_scenic_score([], _, Max) ->
    Max;
max_scenic_score([H | T], Grid, Max) ->
    case scenic_score(H, Grid) of
        S when S > Max -> max_scenic_score(T, Grid, S);
        _ -> max_scenic_score(T, Grid, Max)
    end.

scenic_score(Tree, Grid) ->
    scenic_score(left, Tree, Grid, 0)
    * scenic_score(right, Tree, Grid, 0)
    * scenic_score(top, Tree, Grid, 0)
    * scenic_score(bottom, Tree, Grid, 0).

scenic_score(Direction, {Pos, Height}, Grid, Score) ->
    NeighbourPos = neighbour(Direction, Pos),
    case maps:get(NeighbourPos, Grid, finished) of
        finished ->
            Score;
        Neighbour when Neighbour >= Height ->
            Score + 1;
        _ ->
            scenic_score(Direction, {NeighbourPos, Height}, Grid, Score + 1)
    end.

neighbour(left, {X, Y}) -> {X - 1, Y};
neighbour(right, {X, Y}) -> {X + 1, Y};
neighbour(top, {X, Y}) -> {X, Y + 1};
neighbour(bottom, {X, Y}) -> {X, Y - 1}.

input() ->
    {ok, I} = file:read_file(input),
    parse_grid(binary:split(I, <<"\n">>, [global]), #{}, 0).

parse_grid([], Grid, _) ->
    Grid;
parse_grid([Line | Rest], Grid, Row) ->
    {NewGrid, _} = lists:foldl(fun (C, {G, Column}) ->
                                   V = C - $1 + 1,
                                   {maps:put({Row, Column}, V, G), Column + 1}
                               end, {Grid, 0}, binary_to_list(Line)),
    parse_grid(Rest, NewGrid, Row + 1).