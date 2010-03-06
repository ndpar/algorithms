%% Problem
%% ---------------------
%% The prime factors of 13195 are 5, 7, 13 and 29.
%% What is the largest prime factor of the number 600851475143?
%% ---------------------

-module(p003).
-export([solve/0]).
-include_lib("eunit/include/eunit.hrl").

%% Solution
%% ---------------------
%% Inspired by:
%% http://thetaoishere.blogspot.com/2008/05/largest-prime-factor-of-number.html
%% ---------------------

solve() -> lpf(600851475143).

lpf(1) -> 1;
lpf(2) -> 2;
lpf(N) when N rem 2 == 0 -> lpf(erlang:max(2, N div 2));
lpf(N) -> lpf(3, trunc(math:sqrt(N)), N).

lpf(I, Sn, N) when I > Sn -> N;
lpf(I, _S, N) when N rem I == 0 -> erlang:max(I, lpf(N div I));
lpf(I, Sn, N) -> lpf(I + 2, Sn, N).


%% Tests

lpf_1_test() ->
    ?assertEqual(1, lpf(1)).

lpf_2_test() ->
    ?assertEqual(2, lpf(2)).

lpf_3_test() ->
    ?assertEqual(3, lpf(3)).

lpf_16_test() ->
    ?assertEqual(2, lpf(16)).

lpf_20_test() ->
    ?assertEqual(5, lpf(20)).

lpf_17_test() ->
    ?assertEqual(17, lpf(17)).
