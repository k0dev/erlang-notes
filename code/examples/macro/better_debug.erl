-module(better_debug).
-export([divide/2]).

% Esempio di esecuzione
% > c(better_debug).            
% {ok,better_debug}
% > better_debug:divide(20, 10).
% [Line 4] fun better_debug:divide/2([20,10])
% 2.0


divide(A, B) -> debug_call_print(fun better_debug:divide/2, ?LINE, [A, B]),
                A / B.

debug_call_print(Msg, Line, Args) -> io:format("[Line ~p] ~p(~p)~n", [Line, Msg, Args]).
