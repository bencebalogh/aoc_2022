-module(aoc).
-export([part1/0]).
-export([part2/0]).

-define(TOTAL_DISK, 70000000).
-define(REQUIRED, 30000000).

part1() ->
    Sizes = size([], input(), #{}),
    lists:sum(lists:filter(fun (Size) -> Size =< 100000 end, maps:values(Sizes))).

part2() ->
    Sizes = size([], input(), #{}),
    Free = ?TOTAL_DISK - maps:get([], Sizes),
    Need = ?REQUIRED - Free,
    lists:min(lists:filter(fun (Size) -> Size >= Need end, maps:values(Sizes))).

size(Path, Structure, Agg) ->
    Contents = maps:get(Path, Structure),
    NewAgg = lists:foldl(fun
                             ({dir, D}, A) ->
                                 SubPath = Path ++ [D],
                                 PrevSize =  maps:get(Path, A, 0),
                                 SubAgg = size(SubPath, Structure, A),
                                 SubSize = maps:get(SubPath, SubAgg, 0),
                                 SubAgg#{Path => SubSize + PrevSize};
                             ({file, F}, A) ->
                                 A#{Path => maps:get(Path, A, 0) + F}
                         end, Agg, Contents),
    NewAgg.

input() ->
    {ok, I} = file:read_file(input),
    parse_line(binary:split(I, <<"\n">>, [global]), [], #{[] => []}).

parse_line([], _, Parsed) ->
    Parsed;
parse_line([<<"$ cd /">> | Rest], _, Parsed) ->
    parse_line(Rest, [], Parsed);
parse_line([<<"$ cd ..">> | Rest], Path, Parsed) ->
    NewPath = lists:sublist(Path, length(Path) - 1),
    parse_line(Rest, NewPath, Parsed);
parse_line([<<"$ cd ", Dir/binary>> | Rest], Path, Parsed) ->
    NewPath = Path ++ [Dir],
    parse_line(Rest, NewPath, Parsed);
parse_line([<<"$ ls">> | Rest], Path, Parsed) ->
    parse_line(Rest, Path, Parsed);
parse_line([<<"dir ", Dir/binary>> | Rest], Path, Parsed) ->
    NewParsed = Parsed#{Path => [{dir, Dir} | maps:get(Path, Parsed, [])]},
    parse_line(Rest, Path, NewParsed);
parse_line([File | Rest], Path, Parsed) ->
    [Sizebin, _Name] = binary:split(File, <<" ">>),
    Size = binary_to_integer(Sizebin),
    NewParsed = Parsed#{Path => [{file, Size} | maps:get(Path, Parsed, [])]},
    parse_line(Rest, Path, NewParsed).