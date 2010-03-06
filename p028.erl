%% Problem
%% ---------------------
%% What is the sum of the numbers on the diagonals in a 1001 by 1001
%% spiral formed in the same way?
%% ---------------------

-module(p028).
-export([solve/0]).


%% N^2 + (N^2-N+1) + (N^2-2N+2) + (N^2-3N+3) = 4N^2-6N+6
%%
solve() -> 1 + lists:sum([ 4*N*N-6*N+6 || N <- lists:seq(3, 1001, 2) ]).
