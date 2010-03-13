-module(mymath).
-export([c/2, ds/1, factorial/1, is_palindrome/1, lcm/2, perms/1, pow/2, primes_upto/1, prod/1]).
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


%% Binomial coefficients
%%
c(N, N) -> 1;
c(N, M) when M < N-M -> c(N, N-M);
c(N, M) -> prod(lists:seq(M+1, N)) div factorial(N-M).


%% Product of numbers in the list
%%
prod(List) -> lists:foldl(fun erlang:'*'/2, 1, List).

factorial(0) -> 1;
factorial(N) -> prod(lists:seq(1,N)).


%% Sum of digits in the given integer
%%
ds(M) -> lists:foldl(fun(N, Sum) -> Sum + N - $0 end, 0, integer_to_list(M)).


%% Integer power of another integer
%%
pow(N, 1) -> N;
pow(N, E) -> N * pow(N, E-1).


%% Calculates all permutations for elements from given list
%%
perms([]) -> [[]];
perms(L) -> [[H|T] || H <- L, T <- perms(L--[H])].


%% Returns true if given list or integer is palindrom
%%
is_palindrome([]) -> [];
is_palindrome([X|Xs]) -> [X|Xs] =:= lists:reverse([X|Xs]);
is_palindrome(N) -> is_palindrome(integer_to_list(N)).


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

prod_test() ->
    ?assertEqual(90, prod([2,5,9])).

c_test() ->
    ?assertEqual(3, c(3, 1)).

factorial_test() ->
    ?assertEqual(120, factorial(5)).

ds_test() ->
    ?assertEqual(21, ds(1569)).

pow_test() ->
    ?assertEqual(32, pow(2, 5)).

perms_test() ->
    ?assertEqual(["012", "021", "102", "120", "201", "210"], perms("012")).

is_palindrome_int_true_test() ->
    ?assertEqual(true, is_palindrome(9009)).

is_palindrome_int_false_test() ->
    ?assertEqual(false, is_palindrome(9001)).

is_palindrome_list_true_test() ->
    ?assertEqual(true, is_palindrome("1001001001")).

is_palindrome_list_false_test() ->
    ?assertEqual(false, is_palindrome("1001001011")).
