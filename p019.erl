%% Problem
%% ---------------------
%% How many Sundays fell on the first of the month during
%% the twentieth century (1 Jan 1901 to 31 Dec 2000)?
%% ---------------------

-module(p019).
-export([solve/0]).


solve() -> length([ {M, Y} || Y <- lists:seq(1901, 2000), M <- lists:seq(1, 12), calendar:day_of_the_week(Y, M, 1) =:= 7 ]).