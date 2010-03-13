%% Problem
%% ---------------------
%% 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
%%
%% Find the sum of all numbers which are equal to the sum of the factorial of their digits.
%%
%% Note: as 1! = 1 and 2! = 2 are not sums they are not included.
%% ---------------------

-module(p034).
-export([solve/0]).
-include_lib("eunit/include/eunit.hrl").


%% Some thoughts:
%% 9! = 362880; 7 times 9! is less than 9999999.
%% Brute force with that bound is very slow.
%% To find more accurate upper bound solve the equation:
%% x = 9! * ln(x) => x = 2309171
%% Even with this bound brute force is still slow.
%%
solve() -> lists:sum([ N || N <- lists:seq(3, 2309171), N =:= sum(N) ]).

sum(N) -> lists:sum([ mymath:factorial(M-$0) || M <- integer_to_list(N) ]).


sum_test() ->
    ?assertEqual(145, sum(145)).


% See also:
% http://mathworld.wolfram.com/Factorion.html
