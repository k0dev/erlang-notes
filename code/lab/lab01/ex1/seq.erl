-module(seq).
-export([is_palindrome/1]).

is_palindrome(S) -> % reminder: le stringhe sono liste di int
  L  = string:to_lower(S),
  LF = lists:filter(fun(C) -> (C >= $a) and (C =< $z) end, L),
  LF =:= lists:reverse(LF).
