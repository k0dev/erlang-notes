-module(distributed_process_ring).
-export([start/3, create/3, ring_node/2]).

% In questa versione ogni nodo spawna il suo successivo (tranne l'ultimo, che si "collega" al primo tramite l'atomo registrato)

%%%
% [I'm node n. 1 (my pid is <0.89.0>)] "I have been spawned and i will create my next node"
% [I'm node n. 1 (my pid is <0.89.0>)] "I spawned my next node (his pid is <0.90.0>)"
% [I'm node n. 2 (my pid is <0.90.0>)] "I have been spawned and i will create my next node"
% [I'm node n. 2 (my pid is <0.90.0>)] "I spawned my next node (his pid is <0.91.0>)"
% [I'm node n. 3 (my pid is <0.91.0>)] "I have been spawned and i will create my next node"
% [I'm node n. 3 (my pid is <0.91.0>)] "I spawned my next node (his pid is <0.92.0>)"
% [I'm node n. 4 (my pid is <0.92.0>)] "I have been spawned and i will create my next node"
% [I'm node n. 4 (my pid is <0.92.0>)] "I spawned my next node (his pid is <0.93.0>)"
% [I'm node n. 5 (my pid is <0.93.0>)] "I am the last node, i will connect to the first node"
%%%

start(M, N, Message) -> 
    register(first_node, spawn(distributed_process_ring, create, [1, N, self()])),
    receive
        done -> void
    after(5000) -> exit(timeout)
    end,
    send_messages(1, M, Message),
    first_node ! stop,
    ok.

send_messages(M, M, Msg) ->
    first_node ! {M, Msg};
send_messages(I, M, Msg) -> 
    first_node ! {I, Msg},
    send_messages(I+1, M, Msg).

create(N, N, Pid) ->
    debug_print(N, "I am the last node, i will connect to the first node"),
    Pid ! done,
    ring_node(N, first_node);
create(I, N, Pid) ->
    debug_print(I, "I have been spawned and i will create my next node"),
    NextNode = spawn(distributed_process_ring, create, [I+1, N, Pid]),
    debug_print(I, lists:flatten(io_lib:format("I spawned my next node (his pid is ~p)", [NextNode]))),
    ring_node(I, NextNode).

ring_node(I, NextNode) -> 
    receive
        stop ->
            debug_print(I, "stop"),
            catch NextNode ! stop;
        Msg ->
            debug_print(I, Msg),
            catch NextNode ! Msg,
            ring_node(I, NextNode)
        after(3000) -> debug_print(I, "timeout")
    end.

debug_print(I, Msg) -> io:format("[I'm node n. ~p (my pid is ~p)] ~p~n", [I, self(), Msg]).
