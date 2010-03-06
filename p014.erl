%% Problem
%% ---------------------
%% The following iterative sequence is defined for the set of positive integers:
%%
%% n -> n/2 (n is even)
%% n -> 3n + 1 (n is odd)
%%
%% Using the rule above and starting with 13, we generate the following sequence:
%%
%% 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
%%
%% It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
%% Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
%%
%% Which starting number, under one million, produces the longest chain?
%% ---------------------

-module(p014).
-export([solve/0]).
-include_lib("eunit/include/eunit.hrl").

%%
%% Brute force solution (still relatively fast)
%%
solve() -> lists:max([ {cl(N), N} || N <- lists:seq(2,1000000) ]).

%%
%% Chain length
%%
cl(1) -> 1; 
cl(N) when N rem 2 =:= 0 -> 1 + cl(N div 2); 
cl(N) -> 1 + cl(3 * N + 1).

cl_test() -> ?assertEqual(10, cl(13)).
