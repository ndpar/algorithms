%
% Several algorithms to find n-th Fibonacci number. Inspired by
% http://www.catonmat.net/blog/mit-introduction-to-algorithms-part-two
% http://en.wikipedia.org/wiki/Dynamic_programming
%

-module(fibonacci).
-export([naive_recursive/1, bottom_up/1, squaring/1]).

-include_lib("eunit/include/eunit.hrl").

% Exponential:
% T(n) = Ω(Φ^n), Φ is Golden ratio

naive_recursive(0) -> 0;
naive_recursive(1) -> 1;
naive_recursive(N) -> naive_recursive(N-1) + naive_recursive(N-2).

naive_recursive_test() -> ?assertEqual(8, naive_recursive(6)).


% Linear:
% T(n) = Θ(n)
% Dynamic programming technique

bottom_up(N) -> bottom_up(N, 1, {1,0}).
bottom_up(N, N, {X,_}) -> X;
bottom_up(N, M, {X,Y}) -> bottom_up(N, M+1, {X+Y,X}).

bottom_up_test() -> ?assertEqual(8, bottom_up(6)).


% Logarithmic:
% T(n) = Θ(log n)
% http://en.wikipedia.org/wiki/Fibonacci_number#Matrix_form

squaring(N) -> [_,X,X,_] = power([1,1,1,0], N), X.

squaring_test() -> ?assertEqual(8, squaring(6)).


% Matrix exponentiation A^n; logarithmic algorithm Θ(log n)

power(Matrix, 1) -> Matrix;
power(Matrix, N) when N rem 2 =:= 0 ->
    M2 = power(Matrix, N div 2),
    mult(M2, M2);
power(Matrix, N) ->
    M2 = power(Matrix, N div 2),
    mult(mult(M2, M2), Matrix).

mult([A,B,C,D], [K,L,M,N]) -> [A*K+B*M, A*L+B*N, C*K+D*M, C*L+D*N].
