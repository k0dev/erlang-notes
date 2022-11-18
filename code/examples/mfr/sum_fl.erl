-module(sum_fl).
-export([sum/1]).

% Esempio di esecuzione:
% > c(sum_fl).
% {ok,sum_fl}
% > sum_fl:sum([1,2,3,4,5]).
% 15

sum(L) -> lists:foldl(fun(A, B) -> A + B end, 0, L).
