%% Problem
%% ---------------------
%% The decimal number, 585 = 1001001001 (binary), is palindromic in both bases.
%%
%% Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
%% ---------------------

-module(p036).
-export([solve/0]).


%% Brute force is pretty fast
%%
solve() -> lists:sum([ N || N <- lists:seq(1, 1000000-1), mymath:is_palindrome(N), mymath:is_palindrome(bin(N)) ]).

bin(N) -> hd(io_lib:format("~.2B", [N])).
