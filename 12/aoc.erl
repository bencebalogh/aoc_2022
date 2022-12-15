-module(aoc).
-export([part1/0]).
-export([part2/0]).

part1() ->
    {Grid, Start, End} = input(),
    shortest_path(Grid, End, ordsets:from_list([{0, Start}]), #{}).

part2() ->
    {Grid, _, End} = input(),
    Starts = lists:filter(fun ({_, V}) -> V == 0 end, maps:to_list(Grid)),
    lists:min(lists:map(fun ({Start, 0}) ->
                  shortest_path(Grid, End, ordsets:from_list([{0, Start}]), #{})
              end, Starts)).

shortest_path(_, End, [{Count, End} | _], _) ->
    Count;
shortest_path(_, _, [], _) ->
    infinity;
shortest_path(Grid, End, [{Count, Pos} | Paths], Seen) ->
    NewPaths = lists:filtermap(fun (Option) ->
                                  case maps:get(Option, Seen, infinity) of
                                      I when I =< Count + 1 -> false;
                                      _ -> {true, {Count + 1, Option}}
                                  end
                         end, options(Pos, Grid)),
    shortest_path(Grid, End, ordsets:union(Paths, ordsets:from_list(NewPaths)), maps:put(Pos, Count, Seen)).

options(Pos, Grid) ->
    Height = maps:get(Pos, Grid),
    lists:filtermap(fun
                    (Neighbour) when is_map_key(Neighbour, Grid) ->
                        N = maps:get(Neighbour, Grid),
                        case N - Height of 
                            I when I =< 1 -> {true, Neighbour};
                            _ -> false
                        end;
                    (_) ->
                        false
                end, neighbours(Pos)).

neighbours({X, Y}) -> [{X + 1, Y}, {X - 1, Y}, {X, Y + 1}, {X, Y - 1}].

input() ->
    {ok, I} = file:read_file(input),
    Lines = binary:split(I, <<"\n">>, [global]),
    lists:foldl(fun parse_line/2, {#{}, unknown, unknown}, lists:zip(lists:seq(0, length(Lines) - 1), Lines)).

parse_line({Y, Line}, Agg) ->
    LineList = binary_to_list(Line),
    lists:foldl(fun
                    ({X, $S}, {Grid, _, EndPos}) ->
                        {maps:put({X, Y}, 0, Grid), {X, Y}, EndPos};
                    ({X, $E}, {Grid, StartPos, _}) ->
                        {maps:put({X, Y}, 25, Grid), StartPos, {X, Y}};
                    ({X, H}, {Grid, StartPos, EndPos}) ->
                        {maps:put({X, Y}, H - 97, Grid), StartPos, EndPos}
                end, Agg, lists:zip(lists:seq(0, length(LineList) - 1), LineList)).