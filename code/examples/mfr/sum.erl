-module(sum).
-export([sum/1]).

% Esempio di esecuzione
% > c(sum).
% {ok,sum}
% > sum:sum([1,2,3,4,5]).
% 15

sum([])    -> 0;
sum([H|T]) -> H + sum(T).
