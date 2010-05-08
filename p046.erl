%% Problem
%% ---------------------
%% It was proposed by Christian Goldbach that every odd composite number
%% can be written as the sum of a prime and twice a square.
%%
%% 9 = 7 + 2x1^2
%% 15 = 7 + 2x2^2
%% 21 = 3 + 2x3^2
%% 25 = 7 + 2x3^2
%% 27 = 19 + 2x2^2
%% 33 = 31 + 2x1^2
%%
%% It turns out that the conjecture was false.
%%
%% What is the smallest odd composite that cannot be written as the sum
%% of a prime and twice a square?
%% ---------------------

-module(p046).
-export([solve/0]).
-define(MAX, 6000).


%% Very slow brute force
%%
solve() ->
    [ X || X <- candidates(), is_stern(X) ].

candidates() ->
    [ X || X <- lists:seq(9, ?MAX) -- mymath:primes_upto(?MAX), X rem 2 > 0].

is_stern(N) ->
    length([ {P, B} || P <- mymath:primes_upto(N-2), B <- lists:seq(1, trunc(math:sqrt(N))), N == P + 2*B*B ]) == 0.


% See also http://learning.physics.iastate.edu/hodges/mm-1.pdf