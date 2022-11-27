-module(centralized_process_ring).
-export([start/3]).

% A quale processo collego il primo nodo creato? Ad un atomo che registerò in futuro.
% (con collegare non si intende un link di erlang, ma piuttosto un collegamento simile a quello delle linked list)
% P_n   ---> first_node (non ancora spawnato)
% P_n-1 ---> P_n
% ..
% P_1   ---> P_2
% register(first_node, P_1)
% quindi l'anello si completa.

start(M, N, Message) -> 
  create(N),                      % creo l'anello
  send_messages(M, {1, Message}), % introduco i messaggi
  timer:sleep(10),                % aspetto un po' per farli circolare
  first_node ! stop,              % mando il messaggio di stop
  ok.

send_messages(0, _) -> ok;
send_messages(M, {Index, Message}) ->
  first_node ! {0, Index, Message},
  send_messages(M-1, {Index+1, Message}).

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
        debug_print("stopping"),
        exit(stop);
    {ForwardedCount, Index, Message} ->
        debug_print(ForwardedCount, Index, Message, NextNode),
        catch NextNode ! {ForwardedCount+1, Index, Message},
        ring_node(NextNode)
  end.

debug_print(Count, Index, Msg, To) -> 
  io:format("[From: ~p] -> [To: ~p] - ~p:~p (forwarded ~p times)~n", [self(), To, Index, Msg, Count]).
debug_print(Msg, To) -> io:format("[From: ~p] -> [To: ~p] - ~p~n", [self(), To, Msg]).
debug_print(Msg) -> io:format("[Pid: ~p] ~p~n", [self(), Msg]).
