%% Problem
%% ---------------------
%% By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13,
%% we can see that the 6th prime is 13.
%%
%% What is the 10001st prime number?
%% ---------------------

-module(p007).
-include_lib("eunit/include/eunit.hrl").

%% Solution
%% ---------------------
%% Inspired by:
%% http://basildoncoder.com/blog/2008/10/26/project-euler-problem-7/
%% ---------------------

find_prime(N) ->
    lists:nth(N, sieve_with_atleast_n_primes(N)).

sieve_with_atleast_n_primes(N) ->
    mymath:primes_upto(upper_bound_estimate(N)).

upper_bound_estimate(N) ->
    trunc(N * math:log(N) + N * math:log(math:log(N))).


%% Tests

find_prime_10_test() ->
    ?assertEqual(29, find_prime(10)).

upper_bound_estimate_test() ->
    ?assertEqual(114319, upper_bound_estimate(10001)).

find_prime_10001_test() ->
    ?assertEqual(104743, find_prime(10001)).


%% See also:
%% http://primes.utm.edu/lists/small/