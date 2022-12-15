-module(aoc).
-export([part1/0]).
-export([part2/0]).

part1() ->
    Input = input(),
    lists:foldl(fun ({Index, [Pair1, Pair2]}, Agg) ->
                    case valid(Pair1, Pair2) of
                        true -> Agg + Index;
                        false -> Agg
                    end
                end, 0, lists:zip(lists:seq(1, length(Input)), Input)).

part2() ->
    Sorted = lists:sort(fun valid/2, lists:flatmap(fun (I) -> I end, input()) ++ [[[2]], [[6]]]),
    {A, B} = lists:foldl(fun
                             ({I, [[2]]}, {_, B}) -> {I, B};
                             ({I, [[6]]}, {A, _}) -> {A, I};
                             (_, Agg) -> Agg
                         end, {unknown, unknown}, lists:zip(lists:seq(1, length(Sorted)), Sorted)),
    A * B.

valid(H1, H2) when is_integer(H1) andalso is_integer(H2) andalso H1 < H2 ->
    true;
valid(H, H) when is_integer(H) ->
    next;
valid(H1, H2) when is_integer(H1) andalso is_integer(H2) ->
    false;
valid(P1, P2) when is_integer(P1) andalso is_list(P2) ->
    valid([P1], P2);
valid(P1, P2) when is_list(P1) andalso is_integer(P2) ->
    valid(P1, [P2]);
valid([], [_ | _]) ->
    true;
valid([_ | _], []) ->
    false;
valid([], []) ->
    next;
valid([H1 | T1], [H2 | T2]) ->
    case valid(H1, H2) of
        next -> valid(T1, T2);
        Result -> Result
    end.

input() ->
    {ok, I} = file:read_file(input),
    parse_pair(binary:split(I, <<"\n\n">>, [global]), []).

parse_pair([], Agg) ->
    lists:reverse(Agg);
parse_pair([H | T], Agg) ->
    [F, S] = binary:split(H, <<"\n">>),
    parse_pair(T, [[eval(F), eval(S)] | Agg]).

eval(Pair) ->
    {ok, Tokens, _} = erl_scan:string(binary_to_list(Pair) ++ "."),
    {ok, Done} = erl_parse:parse_term(Tokens),
    Done.