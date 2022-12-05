-module(onexit).
-export([execute/2]).

%% Primo esempio di esecuzione
% > c(onexit).
% {ok,onexit}
% > spawn(fun() -> receive X -> 10/X end end).
% <0.92.0>
% > onexit:execute(<0.92.0>, fun(Why) -> io:format("monitored pid terminated with reason: ~p~n", [Why]) end).
% <0.94.0>
% > <0.92.0> ! 0.
% monitored pid terminated with reason: {badarith,
%                                        [{erlang,'/',
%                                          [10,0],
%                                          [{error_info,
%                                            #{module => erl_erts_errors}}]}]}
% =ERROR REPORT==== 5-Dec-2022::18:24:05.725906 ===
% Error in process <0.92.0> with exit value:
% {badarith,[{erlang,'/',[10,0],[{error_info,#{module => erl_erts_errors}}]}]}
% 
% 0

%% Secondo esempio di esecuzione
% > spawn(fun() -> receive X -> 10/X end end).                 <0.97.0>                                      
% > onexit:execute(<0.97.0>, fun(Why) -> io:format("monitored pid terminated with reason: ~p~n", [Why]) end).
% <0.99.0>
% > <0.97.0> ! 5.                                              
% monitored pid terminated with reason: normal  
% 5

% Questa funzione esegue F(Why) quando Pid termina con motivo Why
execute(Pid, F) ->
    spawn(fun() ->
        Ref = monitor(process, Pid),
        receive
            {'DOWN', Ref, process, Pid, Why} -> F(Why)
        end
    end).
