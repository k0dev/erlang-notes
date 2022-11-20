-module(trap).
-export([log_exit/1]).

log_exit(Pid) ->
  spawn(fun() ->
    process_flag(trap_exit, true),
    link(Pid),
    receive 
      {'EXIT', Pid, Why} -> io:format("Process ~p exited with reason ~p~n", [Pid, Why])
    end
  end).

% Esempio di esecuzione

%%% Uscita senza errore %%%
% > spawn(fun() -> receive L -> list_to_tuple(L) end end).
% <0.95.0>
% > trap:log_exit(<0.95.0>).                            
% <0.97.0>
% > <0.95.0> ! [1,2,3,4,5].                             
% Process <0.95.0> exited with reason normal
% [1,2,3,4,5]
%
%
%%% Uscita con errore %%%
% spawn(fun() -> receive L -> list_to_tuple(L) end end).
% <0.101.0>
% > trap:log_exit(<0.101.0>).                          
% <0.103.0>
% > <0.101.0> ! not_a_list.                            
% Process <0.101.0> exited with reason {badarg,[{erlang,list_to_tuple, [not_a_list], [{error_info, #{module => erl_erts_errors}}]}]}
%
% =ERROR REPORT==== 20-Nov-2022::15:41:50.683629 ===
% Error in process <0.101.0> with exit value:
% {badarg,[{erlang,list_to_tuple,
%                 [not_a_list],
%                 [{error_info,#{module => erl_erts_errors}}]}]}
%
% not_a_list
