-module(odd_even).
-export([split/1]).

split(L) -> split([], [], L).

split(Evens, Odds, [])    -> {lists:reverse(Evens), lists:reverse(Odds)};
split(Evens, Odds, [H|T]) ->
  case H rem 2 of
    0 -> split([H|Evens], Odds, T);
    1 -> split(Evens, [H|Odds], T)
  end.
