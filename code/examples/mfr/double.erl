-module(double).
-export([double_list/1]).

% Esempio di esecuzione
% 1> c(double).
% {ok,double}
% 2> double:double_list([1,2,3,4]).
% [2,4,6,8]

double_list(L) -> lists:map(fun double/1, L).
double(X) -> X*2.
