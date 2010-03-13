%% Problem
%% ---------------------
%% An irrational decimal fraction is created by concatenating the positive integers:
%%
%% 0.123456789101112131415161718192021...
%%
%% It can be seen that the 12th digit of the fractional part is 1.
%%
%% If dn represents the nth digit of the fractional part, find the value of the following expression.
%%
%% d1 x d10 x d100 x d1000 x d10000 x d100000 x d1000000
%% ---------------------

-module(p040).
-export([solve/0]).


solve() ->
    Number = lists:flatten([ integer_to_list(X) || X <- lists:seq(1,230000) ]), 
    mymath:prod([ lists:nth(N,Number) - $0 || N <- [1,10,100,1000,10000,100000,1000000] ]).
