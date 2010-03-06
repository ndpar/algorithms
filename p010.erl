%% Problem
%% ---------------------
%% The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
%%
%% Find the sum of all the primes below two million.
%% ---------------------

-module(p010).
-export([solve/0]).


solve() -> sum_primes(2000000).

sum_primes(N) -> lists:sum(mymath:primes_upto(N)).
