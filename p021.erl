%% Problem
%% ---------------------
%% Evaluate the sum of all the amicable numbers under 10000.
%% ---------------------

-module(p021).
-export([solve/0]).


%% Brute force works relatively fast.
%%
solve() ->
    Tuples = candidates(),
    Amis = [ {N, D} || {N, D} <- Tuples, {N1, D1} <- Tuples, N1 =:= D, D1 =:= N ],
    lists:foldl(fun({N, _}, Sum) -> N + Sum end, 0, Amis).

candidates() ->
    lists:filter(fun({N, D}) -> (D > 1) and (N =/= D) end, all_tuples()).

all_tuples() ->
    [ {N, d(N)} || N <- lists:seq(2, 10000-1) ].

d(N) ->
    lists:sum(prop_devisors(N)).

prop_devisors(N) ->
    [ M || M <- lists:seq(1, N-1), N rem M =:= 0 ].


% See also sigma function
% http://mathschallenge.net/index.php?section=faq&ref=number/sum_of_divisors