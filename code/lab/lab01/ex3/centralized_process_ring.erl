-module(centralized_process_ring).
-export([start/3]).

start(M, N, Message) -> 
  create(N),
  send_messages(M, Message),
  ok.

send_messages(0, _) -> ok;
send_messages(M, Message) ->
  first_node ! Message,
  send_messages(M-1, Message),
  first_node ! stop.

create(N) ->
  Pid = spawn(fun() -> ring_node(first_node) end),
  io:format("[master] created node ~p with pid ~p chained to node 1 (first_node, not yet created)~n", [N, Pid]),
  create(Pid, N-1).

create(NextNode, 1) -> 
  register(first_node, spawn(fun() -> ring_node(NextNode) end)),
  io:format("[master] created node 1 (first_node) with pid ~p chained to node 2 with pid ~p~n", [whereis(first_node), NextNode]);
create(NextNode, N) ->
  Pid = spawn(fun() -> ring_node(NextNode) end),
  io:format("[master] created node ~p with pid ~p chained to node ~p with pid ~p~n", [N, Pid, N+1, NextNode]),
  create(Pid, N-1).

ring_node(NextNode) ->
  receive
    stop ->
      debug_print("stop", NextNode),
      % uso catch per evitare un eccezione badarg nel momento in cui NextNode == first_node
      catch NextNode ! stop,
      debug_print("stopping");
    Msg ->
      debug_print(Msg, NextNode),
      catch NextNode ! Msg,
      ring_node(NextNode)
  end.

debug_print(Msg, To) -> io:format("[From: ~p] -> [To: ~p] - ~p~n", [self(), To, Msg]).
debug_print(Msg) -> io:format("[Pid: ~p] ~p~n", [self(), Msg]).

% Esempio output creazione:
% 1> c(centralized_process_ring).
% {ok,centralized_process_ring}
% 2> centralized_process_ring:start(0, 5, x).
% [master] created node 5 with pid <0.89.0> chained to node 1 (first_node, not yet created)
% [master] created node 4 with pid <0.90.0> chained to node 5 with pid <0.89.0>
% [master] created node 3 with pid <0.91.0> chained to node 4 with pid <0.90.0>
% [master] created node 2 with pid <0.92.0> chained to node 3 with pid <0.91.0>
% [master] created node 1 (first_node) with pid <0.93.0> chained to node 2 with pid <0.92.0>
% ok
