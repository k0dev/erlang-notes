-module(adder_server).
-export([start/0, rpc/2, loop/0]).

% Esempio di esecuzione
% > c(adder_server).
% {ok,adder_server}
% > Pid = adder_server:start().
% <0.97.0>
% > adder_server:rpc(Pid, {99, 1}).
% 100
% > adder_server:rpc(Pid, {2, 10}).
% 12
% > Pid ! stop.                    
% stop
% > adder_server:rpc(Pid, {2, 10}).
% ** exception exit: timeout
%     in function  adder_server:rpc/2 (adder_server.erl, ...


% Server start
start() -> spawn(adder_server, loop, []).

% Wrapper per il client. Il client può chiamare questa funzione invece di dover
% scrivere ogni volta una receive.
rpc(Pid, Msg) ->
	Pid ! {self(), Msg}, % Inoltra il messaggio al server aggiungendo il pid del chiamante
	receive % Aspetta una risposta dal server
		% pattern match su Pid così prendo solo il messaggio che arriva da quel particolare server
		% e non da un processo qualsiasi che ha mandato un messaggio al client
		{Pid, Any} -> Any % restituisco il risultato al chiamante (client)
	after(5000) -> exit(timeout)
	end.

% Server loop
loop() ->
  receive
    {Pid, {A, B}} ->
        Total = A+B,
        Pid ! {self(), Total}, % Includo anche il mio pid così il client sa che il messaggio arriva da questo server
        loop();
    stop -> void;
    Other -> io:format("received unknown message: ~p~n", [Other]),
	     loop()
  end.
