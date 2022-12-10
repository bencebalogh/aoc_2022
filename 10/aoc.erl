-module(aoc).
-export([part1/0]).
-export([part2/0]).

part1() ->
    run(input(), 1, 1, [], 0, fun sum/3).

part2() ->
    _ = run(input(), 1, 1, [], <<"">>, fun row/3).

run([noop | Rest], X, Index, Cmds, Agg, AggFn) ->
    {Incr, NewCmds} = run_command(Cmds ++ [{0, 1}]),
    run(Rest, X + Incr, Index + 1, NewCmds, AggFn(Agg, Index, X), AggFn);
run([{addx, Y} | Rest], X, Index, Cmds, Agg, AggFn) ->
    {Incr, NewCmds} = run_command(Cmds ++ [{Y, 0}]),
    run(Rest, X + Incr, Index + 1, NewCmds, AggFn(Agg, Index, X), AggFn);
run([], X, Index, [], Agg, AggFn) ->
    AggFn(Agg, Index, X);
run([], X, Index, Cmds, Agg, AggFn) ->
    {Incr, NewCmds} = run_command(Cmds),
    run([], X + Incr, Index + 1, NewCmds, AggFn(Agg, Index, X), AggFn).

run_command([]) -> {0, []};
run_command([{C, 1} | Rest]) -> {C, Rest};
run_command([{C, 0} | Rest]) -> {0, [{C, 1} | Rest]}.

sum(Sum, Index, X) ->
    case Index rem 40 of
        20 ->
            Sum + (X * Index);
        _ ->
            Sum
    end.

row(Row, Index, X) ->
    Pixel = pixel(X + 1, Index),
    NewRow = <<Row/binary, Pixel/binary>>,
    case Index rem 40 of
        0 ->
            io:format("~p ~n", [NewRow]),
            <<"">>;
        _ ->
            NewRow
    end.

pixel(X, Index) when (Index rem 40) == X -> <<"#">>;
pixel(X, Index) when (Index rem 40) - 1 == X -> <<"#">>;
pixel(X, Index) when (Index rem 40) + 1 == X -> <<"#">>;
pixel(_, _) -> <<".">>.

input() ->
    {ok, I} = file:read_file(input),
    lists:map(fun parse_line/1 ,binary:split(I, <<"\n">>, [global])).

parse_line(<<"noop">>) -> noop;
parse_line(<<"addx ", I/binary>>) -> {addx, binary_to_integer(I)}.