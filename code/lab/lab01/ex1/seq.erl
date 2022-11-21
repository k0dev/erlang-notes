-module(seq).
-export([is_palindrome/1, is_an_anagram/2, factors/1, is_proper/1]).

is_letter(C) -> (C >= $a) and (C =< $z) or (C >= $A) and (C =< $Z).

is_palindrome(S) -> 
  L = lists:filter(fun is_letter/1, S),
  string:equal(L, lists:reverse(L), true).  % string:equal(A, B, IgnoreCase) -> boolean()

is_an_anagram(S, Dict) ->
  SS = lists:sort(S),
  lists:any(fun(Candidate) -> SS =:= lists:sort(Candidate) end, Dict).


is_prime(N) -> [X || X <- lists:seq(2, trunc(math:sqrt(N))), N rem X =:= 0] =:= [].

factors(N) ->
  Candidates = [X || X <- lists:seq(2, N), is_prime(X)],
  factors(N, [], Candidates).

factors(1, Acc, _)       -> lists:reverse(Acc);
factors(N, Acc, [H|_]=L) -> factors(N rem H, N, Acc, L).
factors(0, N, Acc, [H|_]=L) -> factors(trunc(N/H), [H|Acc], L);
factors(_, N, Acc, [_|T])   -> factors(N, Acc, T).


is_proper(N) ->
  L = [X || X <- lists:seq(1, trunc(N/2)), N rem X =:= 0],
  lists:foldl(fun(A,B) -> A+B end, 0, L) =:= N.
