%% Problem
%% ---------------------
%% Surprisingly there are only three numbers that can be written as
%% the sum of fourth powers of their digits:
%%
%% 1634 = 1^4 + 6^4 + 3^4 + 4^4
%% 8208 = 8^4 + 2^4 + 0^4 + 8^4
%% 9474 = 9^4 + 4^4 + 7^4 + 4^4
%%
%% The sum of these numbers is 1634 + 8208 + 9474 = 19316.
%%
%% Find the sum of all the numbers that can be written as the sum of
%% fifth powers of their digits.
%% ---------------------

-module(p030).
-export([solve/0]).
-include_lib("eunit/include/eunit.hrl").


%% 6 * 9^5 = 354294
%%
solve() -> lists:sum([ N || N <- lists:seq(11, 354294), value(N) =:= N ]).

value(N) -> lists:sum([ mymath:pow(M-$0, 5) || M <- integer_to_list(N)]).


value_test() ->
    ?assertEqual(33, value(12)).

% [4150,4151,54748,92727,93084,194979]