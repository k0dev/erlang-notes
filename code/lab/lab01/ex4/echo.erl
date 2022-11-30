-module(echo).
-export([start/0, print/1, stop/0]).
-export([server_loop/0]).

start() -> 
  register(echo_server, spawn(?MODULE, server_loop, [])),
  ok.

print(Term) -> 
  echo_server ! {msg, Term},   % in questo modo "echo:print(stop)." non ferma il server ma stampa "stop"
  ok.

stop() -> 
  exit(whereis(echo_server), stopped),
  ok.

server_loop() -> 
  receive
    {msg, M} -> io:format("echo_server: ~p~n", [M]),
                server_loop()
  end.
