%% Problem
%% ---------------------
%% The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
%%
%% Find the sum of all the primes below two million.
%% ---------------------

-module(p010). 
-include_lib("eunit/include/eunit.hrl").

sum_primes(N) -> lists:sum(mymath:primes_upto(N)). 

% Tests

sum_primes_test() ->
    ?assertEqual(142913828922, sum_primes(2000000)).
