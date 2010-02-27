%% Problem
%% ---------------------
%% A Pythagorean triplet is a set of three natural numbers, a < b < c, for which, a^2 + b^2 = c^2
%% For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
%%
%% There exists exactly one Pythagorean triplet for which a + b + c = 1000.
%% Find the product abc.
%% ---------------------

-module(p009).
-include_lib("eunit/include/eunit.hrl").

%% Solution
%% ---------------------
%% Does not work for big P, i.e. 1,000,000
%% ---------------------

triplets(P) ->
    [ {A,B,round(math:sqrt(A*A + B*B))} ||
        A <- lists:seq(1, P-2),
        B <- lists:seq(A+1, P-1),
        P*P =:= 2*(A*P + B*P - A*B)
    ].

%% Tests

triplets_12_test() ->
    ?assertEqual([{3,4,5}], triplets(12)).

triplets_k_test() ->
    ?assertEqual([{200,375,425}], triplets(1000)).
