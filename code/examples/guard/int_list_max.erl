-module(int_list_max).
-export([m/1]).

% Esempio di esecuzione
% > c(int_list_max).
% {ok,int_list_max}
% > int_list_max:m([1,9,23,19,27,31,2,9,87,3]).
% 87
% > int_list_max:m([]).
% undefined
% > int_list_max:m([22]).
% 22

m([])     -> undefined;
m([H|T])  -> m(H, T).

m(M, [])             -> M;
m(M, [H|T]) when H>M -> m(H, T); % usiamo una guard per individuare un nuovo massimo
m(M, [_|T])          -> m(M, T).
