%% Problem
%% ---------------------
%% Find the sum of digits in 100!
%% ---------------------

-module(p020).
-export([solve/0]).

solve() -> mymath:ds(mymath:factorial(100)).
