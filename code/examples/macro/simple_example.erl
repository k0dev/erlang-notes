-module(simple_example).
-export([sum/2]).

-define(point(X, Y), {very_long_atom_name, X, Y}).

% Esempio di esecuzione
% > c(simple_example).
% {ok,simple_example}
% > simple_example:sum({very_long_atom_name, 10, 20}, {very_long_atom_name, 5, 5}).
% {very_long_atom_name,15,25}

sum(?point(X1, Y1), ?point(X2,Y2)) -> ?point(X1+X2, Y1+Y2).
