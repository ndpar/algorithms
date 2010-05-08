%% Problem
%% ---------------------
%% In England the currency is made up of pound, £, and pence, p,
%% and there are eight coins in general circulation:
%%
%% 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
%%
%% It is possible to make £2 in the following way:
%%
%% 1x£1 + 1x50p + 2x20p + 1x5p + 1x2p + 3x1p
%%
%% How many different ways can £2 be made using any number of coins?
%% ---------------------

-module(p031).
-export([solve/0]).

%
% Combinatorial approach
%
solve() -> length([ {N1, N2, N5, N10, N20, N50, N100} ||
    N100 <- lists:seq(0, 2),
    N50 <- lists:seq(0, (200 - N100*100) div 50),
    N20 <- lists:seq(0, (200 - N100*100 - N50*50) div 20),
    N10 <- lists:seq(0, (200 - N100*100 - N50*50 - N20*20) div 10),
    N5 <- lists:seq(0, (200 - N100*100 - N50*50 - N20*20 - N10*10) div 5),
    N2 <- lists:seq(0, (200 - N100*100 - N50*50 - N20*20 - N10*10 - N5*5) div 2),
    N1 <- lists:seq(0, 200 - N100*100 - N50*50 - N20*20 - N10*10 - N5*5 - N2*2),
    N1*1 + N2*2 + N5*5 + N10*10 + N20*20 + N50*50 + N100*100 =:= 200 ]) + 1.
