%% Problem
%% ---------------------
%% The number, 1406357289, is a 0 to 9 pandigital number because it
%% is made up of each of the digits 0 to 9 in some order, but it also
%% has a rather interesting sub-string divisibility property.
%%
%% Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this
%% way, we note the following:
%%
%% d2d3d4=406 is divisible by 2
%% d3d4d5=063 is divisible by 3
%% d4d5d6=635 is divisible by 5
%% d5d6d7=357 is divisible by 7
%% d6d7d8=572 is divisible by 11
%% d7d8d9=728 is divisible by 13
%% d8d9d10=289 is divisible by 17
%%
%% Find the sum of all 0 to 9 pandigital numbers with this property.
%% ---------------------

-module(p043).
-export([solve/0]).
-include_lib("eunit/include/eunit.hrl").


%% Brute force works for less than a minute.
%% perms/1 function is slow.
%%
solve() ->
    lists:sum([ list_to_integer(X) || X <- mymath:perms("0123456789"), condition(X) ]).

%% Explicit and fast but long
condition(X) ->
    condition(X, 2, 2) and
    condition(X, 3, 3) and
    condition(X, 4, 5) and
    condition(X, 5, 7) and
    condition(X, 6, 11) and
    condition(X, 7, 13) and
    condition(X, 8, 17).

% Functional and short but slow
%condition(X) ->
%    lists:foldl(fun([S,P],Acc) -> condition(X,S,P) and Acc end, true,
%        [[2,2],[3,3],[4,5],[5,7],[6,11],[7,13],[8,17]]).

condition(Str, Start, P) ->
    list_to_integer(lists:sublist(Str, Start, 3)) rem P == 0.


cond_test() ->
    ?assertEqual(true, condition("1406357289")).

cond_3_test() ->
    ?assertEqual(true, condition("1406357289", 3, 3)).

cond_7_test() ->
    ?assertEqual(true, condition("1406357289", 5, 7)).

cond_11_test() ->
    ?assertEqual(true, condition("1406357289", 6, 11)).

cond_13_test() ->
    ?assertEqual(true, condition("1406357289", 7, 13)).

cond_17_test() ->
    ?assertEqual(true, condition("1406357289", 8, 17)).

% 1406357289,1430952867,1460357289,4106357289,4130952867,4160357289