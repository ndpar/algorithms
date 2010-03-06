%% Problem
%% ---------------------
%% A permutation is an ordered arrangement of objects. For example,
%% 3124 is one possible permutation of the digits 1, 2, 3 and 4.
%% If all of the permutations are listed numerically or alphabetically,
%% we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:
%%
%% 012   021   102   120   201   210
%%
%% What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
%% ---------------------

-module(p024).
-export([solve/0]).
-include_lib("eunit/include/eunit.hrl").


solve() -> find(1000000, "0123456789").

%% Brute force works relatively fast.
%%
find(Nth, List) -> lists:nth(Nth, perms(List)).

perms([]) -> [[]];
perms(L) -> [[H|T] || H <- L, T <- perms(L--[H])].


perms_test() ->
    ?assertEqual(["012", "021", "102", "120", "201", "210"], perms("012")).

find_test() ->
    ?assertEqual("120", find(4, "012")).
