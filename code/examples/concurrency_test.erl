-module(concurrency_test).
-export([start/2, loop/2]).

              % spawn(module_name, function, param_list)
start(N, Id) -> spawn(concurrency_test, loop, [N, Id]).

loop(0, Id) -> io:format("(pid: ~p - id: ~p) end~n", [self(), Id]);
loop(N, Id) -> io:format("(pid: ~p - id: ~p) ~p~n", [self(), Id, N]), loop(N-1, Id).
