%% Problem
%% ---------------------
%% 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
%%
%% What is the sum of the digits of the number 2^1000?
%% ---------------------

-module(p016).
-export([solve/0]).

solve() -> mymath:ds(trunc(math:pow(2,1000))).
