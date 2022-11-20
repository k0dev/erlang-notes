-module(killer).
-export([start/0]).


start() -> spawn(fun() -> kill() end).

kill() ->
  receive
    stop         -> io:format("the killer has been stopped~n");
    {Pid, force} -> exit(Pid, kill),
                    kill();
    Pid          -> exit(Pid, "You have been killed"),
                    kill()
  end.

% Esempio di esecuzione:
%
% > c(killer).
% {ok,killer}
% > K = killer:start().
% <0.89.0>
% > self().
% <0.82.0>
% > K ! <0.82.0>.
% ** exception exit: "You have been killed"
% > self().
% <0.92.0>
% > is_process_alive(K).
% true
%
% process_flag(trap_exit, true).
% false
% > K ! self().
% <0.92.0>
% > receive {'EXIT', Pid, Why} -> {Pid, Why} end.
% {<0.89.0>,"You have been killed"}

% Dopo aver impostato trap_exit a true, il segnale di uscita inviato dal killer
% viene trasformato in un semplice messaggio inviato alla mailbox.
% Dalla documentazione ufficiale scopriamo però che:
%  "If Reason is the atom kill, that is, if exit(Pid, kill) is called, an untrappable 
%  exit signal is sent to the process that is identified by Pid, which unconditionally 
%  exits with exit reason killed"
% Quindi esiste un segnale di uscita che è unstoppable, infatti:
% > c(killer).
% {ok,killer}
% > K = killer:start().
% <0.89.0>
% > process_flag(trap_exit, true).
% false
% > K ! {self(), force}.
% ** exception exit: killed 
