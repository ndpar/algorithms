%% Problem
%% ---------------------
%% The first two consecutive numbers to have two distinct prime factors are:
%%
%% 14 = 2 x 7
%% 15 = 3 x 5
%%
%% The first three consecutive numbers to have three distinct prime factors are:
%%
%% 644 = 2^2 x 7 x 23
%% 645 = 3 x 5 x 43
%% 646 = 2 x 17 x 19.
%%
%% Find the first four consecutive integers to have four distinct primes factors.
%% What is the first of these numbers?
%% ---------------------

% http://www.research.att.com/~njas/sequences/A075044

-module(p047).
-export([solve/0]).


solve() -> check(37963). % three consecutive with four prime factors

check(N) -> check(N, mymath:primes_upto(135000), []).

check(_, _, [P,Q,R,S]) -> [S,R,Q,P];
check(N, Primes, Acc) ->
    case lists:member(N, Primes) of
        true -> check(N+1, Primes, []);
        _ ->
            F = lists:usort(mymath:factorisation(N, Primes, [])),
            case length(F) of
                4 ->
                    check(N+1, Primes, [N|Acc]);
                _ ->
                    check(N+1, Primes, [])
            end
    end.
