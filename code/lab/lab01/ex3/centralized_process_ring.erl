-module(centralized_process_ring).
-export([start/3]).

create(N) ->
  Pid = spawn(fun() -> ring_node(first_node) end),
  create(Pid, N-1).

create(NextNode, 1) -> register(first_node, spawn(fun() -> ring_node(NextNode) end));
create(NextNode, N) ->
  Pid = spawn(fun() -> ring_node(NextNode) end),
  create(Pid, N-1).

start(_M, N, _Message) -> create(N).

ring_node(NextNode) ->
  receive
    stop ->
      debug_print("stop", NextNode),
      % uso catch per evitare un eccezione badarg nel momento in cui NextNode == first_node
      catch NextNode ! stop,
      debug_print("stopping")
  end.

debug_print(Msg, To) -> io:format("[From: ~p] -> [To: ~p] - ~p~n", [self(), To, Msg]).
debug_print(Msg) -> io:format("[Pid: ~p] ~p~n", [self(), Msg]).

%% Esempio di esecuzione:
% 1> c(centralized_process_ring).
% {ok,centralized_process_ring}
% 2> centralized_process_ring:start(0, 5, unused).
% true
% 3> first_node ! stop.
% [From: <0.93.0>] -> [To: <0.92.0>] - "stop"
% stop
% [Pid: <0.93.0>] "stopping"
% [From: <0.92.0>] -> [To: <0.91.0>] - "stop"
% [Pid: <0.92.0>] "stopping"
% [From: <0.91.0>] -> [To: <0.90.0>] - "stop"
% [Pid: <0.91.0>] "stopping"
% [From: <0.90.0>] -> [To: <0.89.0>] - "stop"
% [Pid: <0.90.0>] "stopping"
% [From: <0.89.0>] -> [To: first_node] - "stop"
% [Pid: <0.89.0>] "stopping"
