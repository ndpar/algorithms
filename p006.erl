%% Problem
%% ---------------------
%% The sum of the squares of the first ten natural numbers is,
%% 1^2 + 2^2 + ... + 10^2 = 385
%% The square of the sum of the first ten natural numbers is,
%% (1 + 2 + ... + 10)^2 = 55^2 = 3025
%% Hence the difference between the sum of the squares of the
%% first ten natural numbers and the square of the sum is 3025 - 385 = 2640.
%%
%% Find the difference between the sum of the squares of the
%% first one hundred natural numbers and the square of the sum.
%% ---------------------

-module(p006).
-include_lib("eunit/include/eunit.hrl").

%% Solution (long)
%% ---------------------

diff(Max) -> 2 * lists:sum([M * N || M <- lists:seq(1, Max), N <- lists:seq(1, Max), M < N]).

%% Solution (fast)
%% ---------------------

diff2(N) -> N * (N + 1) * (3*N + 2) * (N - 1) div 12.


%% Tests

diff_1_test() ->
    ?assertEqual(0, diff(1)).

diff_2_test() ->
    ?assertEqual(4, diff(2)).

diff_10_test() ->
    ?assertEqual(2640, diff(10)).

diff_100_test() ->
    ?assertEqual(25164150, diff(100)).

diff2_100_test() ->
    ?assertEqual(25164150, diff2(100)).
