-module(echo).
-export([start/0, print/1, stop/0]).
-export([server_loop/0]).

start() -> 
  register(echo_server, spawn(?MODULE, server_loop, [])),
  ok.

print(_Term) -> exit(not_implemented_yet).

stop() -> exit(not_implemented_yet).

server_loop() -> 
  receive
    stop -> io:format("echo_server stopping~n");
    M    -> io:format("echo_server: ~p~n", [M]),
            server_loop()
  end.
