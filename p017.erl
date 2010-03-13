%% Problem
%% ---------------------
%% If the numbers 1 to 5 are written out in words: one, two, three, four,
%% five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
%%
%% If all the numbers from 1 to 1000 (one thousand) inclusive were written
%% out in words, how many letters would be used?
%% ---------------------

-module(p017).
-export([solve/0]).


solve() ->
    Units = length("onetwothreefourfivesixseveneightnine"),
    Teens = length("teneleventwelvethirteenfourteenfifteensixteenseventeeneighteennineteen"),
    Ties = length("twentythirtyfortyfiftysixtyseventyeightyninety"),
    Hundred = length("hundred"),
    And = length("and"),
    Thousand = length("onethousand"),
    Thousand + 9*100*Hundred + 9*99*And + 10*10*Ties + 10*Teens + (9*10+100)*Units.
