%% Problem
%% ---------------------
%% A palindromic number reads the same both ways. The largest palindrome made from the
%% product of two 2-digit numbers is 9009 = 91 * 99.
%%
%% Find the largest palindrome made from the product of two 3-digit numbers.
%% ---------------------

-module(p004).
-export([solve/0]).

%% Brute force solution
%% ---------------------

solve() ->
    lists:max(palindromes(lists:seq(100, 999))).

palindromes(Factors) ->
    [M * N || M <- Factors, N <- Factors, mymath:is_palindrome(M * N)].
