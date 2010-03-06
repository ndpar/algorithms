%% Problem
%% ---------------------
%% The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
%%
%% Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
%% ---------------------

-module(p048).
-export([solve/0]).


%% Brute force works fast.
%%
solve() -> solve(10000000000).
solve(D) -> lists:sum([ pow(N, N) rem D || N <- lists:seq(1, 1000)]) rem D.

%% math:pow/2 doesn't work for N > 143
%%
pow(X, N) -> power(X, N, 1).
power(_, 0, Acc) -> Acc;
power(X, N, Acc) -> power(X, N - 1, X * Acc).