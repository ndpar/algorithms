%% Problem
%% ---------------------
%% The 12th Fibonacci number, F12, is the first term to contain three digits.
%%
%% What is the first term in the Fibonacci sequence to contain 1000 digits?
%% ---------------------

-module(p025).
-export([solve/0, solve1/0]).


%% Brute force works fast.
%%
solve() -> fib(2, 1, 1, 1).

fib(N, _, _, L) when L >= 1000 -> N;
fib(N, Fst, Snd, _) -> fib(N+1, Snd, Fst+Snd, length(integer_to_list(Fst+Snd))).


%% Binet's formula works only for N < 1475
%% For greater N it throws bad argument exception
%%
solve1() -> try_next(12).

try_next(N) ->
    case length(integer_to_list(f(N))) >= 1000 of
        true -> N;
        false -> try_next(N+1)
    end.

f(N) -> round((math:pow(phi(), N) - math:pow(1-phi(), N)) / math:sqrt(5)).

phi() -> (1 + math:sqrt(5)) / 2.
