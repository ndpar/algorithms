%% Problem
%% ---------------------
%% The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
%% increases by 3330, is unusual in two ways: (i) each of the three terms
%% are prime, and, (ii) each of the 4-digit numbers are permutations of one another.
%%
%% There are no arithmetic sequences made up of three 1-, 2-, or 3-digit
%% primes, exhibiting this property, but there is one other 4-digit increasing sequence.
%%
%% What 12-digit number do you form by concatenating the three terms in this sequence?
%% ---------------------

-module(p049).
-export([solve/0]).
-include_lib("eunit/include/eunit.hrl").


solve() ->
    Primes = [ X || X <- mymath:primes_upto(9999), 1000 < X ],
    [ {N, M+N, 2*M+N} || N <- Primes, M <- lists:seq(1,8999),
        lists:member(M+N, Primes), permutated(N, M+N),
        lists:member(2*M+N, Primes), permutated(N, 2*M+N)
    ].

permutated(N, M) ->
    lists:sort(integer_to_list(N)) == lists:sort(integer_to_list(M)).


permutated_test() ->
    ?assertEqual(true, permutated(1487, 4817)).
