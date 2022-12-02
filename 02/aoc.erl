-module(aoc).
-export([part1/0]).
-export([part2/0]).

-define(LOOKUP_MATCH, #{
    <<"X">> => <<"A">>,
    <<"Y">> => <<"B">>,
    <<"Z">> => <<"C">>
}).
-define(LOOKUP_OUTCOMES, #{
    <<"A">> => {<<"C">>, <<"B">>},
    <<"B">> => {<<"A">>, <<"C">>},
    <<"C">> => {<<"B">>, <<"A">>}
}).

part1() ->
    lists:sum(lists:map(fun (Round) ->
                            score(transform(Round))
                        end, input())).

part2() ->
    lists:sum(lists:map(fun (Round) ->
                            score(choose_pick(Round))
                        end, input())).

transform(<<Them:1/binary, " ", Me:1/binary>>) ->
    Transformed = maps:get(Me, ?LOOKUP_MATCH),
    <<Them/binary, " ", Transformed/binary>>.

choose_pick(<<Them:1/binary, " ", "Y">>) ->
    <<Them/binary, " ", Them/binary>>;
choose_pick(<<Them:1/binary, " ", "X">>) ->
    {Me, _} = maps:get(Them, ?LOOKUP_OUTCOMES),
    <<Them/binary, " ", Me/binary>>;
choose_pick(<<Them:1/binary, " ", "Z">>) ->
    {_, Me} = maps:get(Them, ?LOOKUP_OUTCOMES),
    <<Them/binary, " ", Me/binary>>.

score(Round) -> pick_score(Round) + outcome_score(Round).

pick_score(<<_Them:1/binary, " ", "A">>) -> 1;
pick_score(<<_Them:1/binary, " ", "B">>) -> 2;
pick_score(<<_Them:1/binary, " ", "C">>) -> 3.

outcome_score(<<"A B">>) -> 6;
outcome_score(<<"B C">>) -> 6;
outcome_score(<<"C A">>) -> 6;
outcome_score(<<Draw:1/binary, " ", Draw:1/binary>>) -> 3;
outcome_score(_) -> 0.

input() ->
    {ok, I} = file:read_file(input),
    binary:split(I, <<"\n">>, [global]).
