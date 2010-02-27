%% Problem
%% ---------------------
%% A palindromic number reads the same both ways. The largest palindrome made from the
%% product of two 2-digit numbers is 9009 = 91 * 99.
%%
%% Find the largest palindrome made from the product of two 3-digit numbers.
%% ---------------------

-module(p004).
-include_lib("eunit/include/eunit.hrl").

%% Solution
%% ---------------------
%% Brute force
%% ---------------------

find_largest_palindrome() ->
    lists:max(palindromes(lists:seq(100, 999))).

palindromes(Factors) ->
    [M * N || M <- Factors, N <- Factors, is_palindrome(M * N)].

is_palindrome(N) ->
    Nl = integer_to_list(N),
    Nl =:= lists:reverse(Nl).


%% Tests

is_palindrome_true_test() ->
    ?assertEqual(true, is_palindrome(9009)).

is_palindrome_false_test() ->
    ?assertEqual(false, is_palindrome(9001)).

find_test() ->
    ?assertEqual(906609, find_largest_palindrome()).
