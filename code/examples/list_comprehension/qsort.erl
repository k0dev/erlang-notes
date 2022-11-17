-module(qsort).
-export([sort/2]).

% Compare deve essere una funzione che dati due interi, A e B, restituisce:
%   un numero > 0 se A>B
%   un numero = 0 se A=B
%   un numero < 0 se A<B

% Con la prima list comprehension prendiamo tutti gli elementi minori o uguali (=<)
% al pivot, mentre con la seconda prendiamo tutti gli elementi maggiori (>).
% Per semplicità l'elemento scelto come pivot è sempre il primo.

% Esempio di esecuzione dalla shell
% > c(qsort).
% {ok,qsort}
% > qsort:sort(fun(A,B) -> A-B end, [19,3,8,7,1,29,3,7]).
% [1,3,3,7,7,8,19,29]

sort(_, [])                -> [];
sort(Compare, [Pivot | T]) -> sort(Compare, [X || X<-T, Compare(X, Pivot) =< 0])
                              ++  % concatena le due liste (simile a @@ di OCaml)
                              [Pivot | sort(Compare, [X || X<-T, Compare(X, Pivot) > 0])].
