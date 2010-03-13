%% Problem
%% ---------------------
%% The number, 197, is called a circular prime because all rotations
%% of the digits: 197, 971, and 719, are themselves prime.
%%
%% There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17,
%% 31, 37, 71, 73, 79, and 97.
%%
%% How many circular primes are there below one million?
%% ---------------------

-module(p035).
-export([solve/0]).
-include_lib("eunit/include/eunit.hrl").


solve() -> find(1000000-1).

find(Max) -> find([ integer_to_list(N) || N <- mymath:primes_upto(Max), no_even(N) ], 0).

no_even(N) -> length([ M || M <- integer_to_list(N), M rem 2 > 0]) =:= length(integer_to_list(N)).

find([], Acc) -> Acc;
find([X|Xs], Acc) ->
    Circle = circle(X),
    Xs1 = Xs -- Circle,
    Acc1 = case length(Xs1) == length([X|Xs]) - length(Circle) of
        true -> Acc + length(Circle);
        false -> Acc
    end,
    find(Xs1, Acc1).


circle(S) -> circle(S, length(S), []).
circle(_, 0, Acc) -> lists:usort(Acc);
circle([H|T], N, Acc) -> circle(T++[H], N-1, [[H|T]|Acc]).


no_even_13_test() ->
    ?assertEqual(true, no_even(13)).

no_even_12_test() ->
    ?assertEqual(false, no_even(12)).

circle_1_test() ->
    ?assertEqual(["1"], circle("1")).

circle_11_test() ->
    ?assertEqual(["11"], circle("11")).

circle_123_test() ->
    ?assertEqual(["123","231","312"], circle("123")).

find_test() ->
    ?assertEqual(13, find(100)).
