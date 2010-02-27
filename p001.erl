%% Problem
%% ---------------------
%% If we list all the natural numbers below 10 that are multiples of 3 or 5,
%% we get 3, 5, 6 and 9. The sum of these multiples is 23.
%%
%% Find the sum of all the multiples of 3 or 5 below 1000.
%% ---------------------

-module(p001).
-include_lib("eunit/include/eunit.hrl").

multiples35(N) ->
    [X || X <- lists:seq(1, N), (X rem 3 =:= 0) or (X rem 5 =:= 0)].

multiples35_test() ->
    ?assertEqual(466, length(multiples35(999))).

result() ->
    lists:sum(multiples35(999)).

result_test() ->
    ?assertEqual(233168, result()).
