-module(aoc).
-export([part1/0]).
-export([part2/0]).

part1() ->
    run(2).

part2() ->
    run(10).

run(RopeLength) ->
    Rope = lists:map(fun (_) -> {0, 0} end, lists:seq(0, RopeLength - 1)),
    length(step(input(), Rope, RopeLength, ordsets:new())).

step([], _, _, Seen) ->
    Seen;
step([{Direction, Count} | Rest], Rope, RopeLength, Seen) ->
    {NewRope, NewSeen} = lists:foldl(fun (_, {R, S}) ->
                    {NewR, NewS} = lists:foldl(fun
                                        (Ri, {[], Si}) ->
                                            {[move(Direction, Ri)], Si};
                                        (Ri, {[BeforeH | _] = Done, Si}) ->
                                            NewRi = follow(BeforeH, Ri),
                                            NewSi = case length(Done) of
                                                I when I == (RopeLength - 1) -> ordsets:add_element(NewRi, Si);
                                                _ -> Si
                                            end,
                                            {[NewRi | Done], NewSi}
                                       end, {[], S}, R),
                    {lists:reverse(NewR), NewS}
                end, {Rope, Seen}, lists:seq(1, Count)),
    step(Rest, NewRope, RopeLength, NewSeen).

move(<<"U">>, {X, Y}) -> {X, Y + 1};
move(<<"D">>, {X, Y}) -> {X, Y - 1};
move(<<"R">>, {X, Y}) -> {X + 1, Y};
move(<<"L">>, {X, Y}) -> {X - 1, Y}.

follow({Hx, Y}, {Tx, Y}) when abs(Hx - Tx) >= 2 ->
    {closer(Hx, Tx), Y};
follow({X, Hy}, {X, Ty}) when abs(Hy - Ty) >= 2 ->
    {X, closer(Hy, Ty)};
follow({Hx, Hy}, {Tx, Ty}) when (abs(Hy - Ty) + abs(Hx - Tx)) >= 3 ->
    {closer(Hx, Tx), closer(Hy, Ty)};
follow(_, T) ->
    T.

closer(A, B) when A > B -> B + 1;
closer(_, B) -> B - 1.

input() ->
    {ok, I} = file:read_file(input),
    lists:map(fun parse_line/1 ,binary:split(I, <<"\n">>, [global])).

parse_line(<<Dir:1/binary, " ", Steps/binary>>) ->
    {Dir, binary_to_integer(Steps)}.