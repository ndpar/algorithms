-module(mymath).
-export([lcm/2, primes_upto/1]).
-include_lib("eunit/include/eunit.hrl").

%% Find all prime numbers upto specified value.
%%
primes_upto(N) -> eratosthenes(math:sqrt(N), lists:seq(2, N)).


%% Functional implementation of Eratosthenes sieve algorithm
%% http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
%%
%% Very clean but very slow. Works relatively fast for N < 100,000
%%
eratosthenes_sieve([]) -> [];
eratosthenes_sieve([P|Xs]) -> [P | eratosthenes_sieve([X || X <- Xs, X rem P > 0])].


%% Functional implementation of Euler sieve algorithm
%%
%% Very clean but very slow. Works relatively fast for N < 10,000
%%
euler_sieve([]) -> [];
euler_sieve([P|Xs]) -> [P | euler_sieve(Xs -- lists:map(fun(X) -> X*P end, [P|Xs]))].


%% Recursion implementation of Eratosthenes sieve algorithm
%% Author: Zac Brown 
%%
%% Not so obvious but very efficient
%%
eratosthenes(Max, [H|T]) when H =< Max -> [H | eratosthenes(Max, sieve([H|T], H))]; 
eratosthenes(_Max, L) -> L. 

sieve([H|T], N) when H rem N =/= 0 -> [H | sieve(T, N)]; 
sieve([_H|T], N) -> sieve(T, N); 
sieve([], _N) -> [].


%% Least common multiple of two integers
%% http://en.wikipedia.org/wiki/Least_common_multiple
%%
lcm(A, B) -> (A * B) div gcd(A, B).


%% Greatest common divisor of two integers
%% http://en.wikipedia.org/wiki/Greatest_common_divisor
%%
gcd(A, B) when A < B -> gcd(B, A);
gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).


%% Tests

primes_upto_30_test() ->
    ?assertEqual([2, 3, 5, 7, 11, 13, 17, 19, 23, 29], primes_upto(30)).

primes_upto_2m_test() ->
    ?assertEqual(1999993, lists:last(primes_upto(2000000))).

euler_sieve_test() ->
    ?assertEqual([2, 3, 5, 7, 11, 13, 17, 19, 23, 29], euler_sieve(lists:seq(2, 30))).

eratosthenes_sieve_test() ->
    ?assertEqual([2, 3, 5, 7, 11, 13, 17, 19, 23, 29], eratosthenes_sieve(lists:seq(2, 30))).

lcm_test() ->
    ?assertEqual(12, lcm(4, 6)).

gcd_test() ->
    ?assertEqual(6, gcd(84, 18)).
