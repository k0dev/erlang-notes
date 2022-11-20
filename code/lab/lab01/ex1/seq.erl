-module(seq).
-export([is_palindrome/1]).

is_letter(C) -> (C >= $a) and (C =< $z) or (C >= $A) and (C =< $Z).

is_palindrome(S) -> 
  L = lists:filter(fun is_letter/1, S),
  string:equal(L, lists:reverse(L), true).  % string:equal(A, B, IgnoreCase) -> boolean()
