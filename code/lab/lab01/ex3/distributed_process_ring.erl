-module(distributed_process_ring).
-export([start/3, ring_node/2]).

% In questa versione ogni nodo spawna il suo successivo (tranne l'ultimo, che si "collega" al primo tramite l'atomo registrato)

start(M, N, Message) -> 
    register(first_node, create(1, N)),
    send_messages(1, M, Message),
    first_node ! stop,
    ok.

send_messages(M, M, Msg) ->
    first_node ! {M, Msg};
send_messages(I, M, Msg) -> 
    first_node ! {I, Msg},
    send_messages(I+1, M, Msg).

create(N, N) ->
    Pid = spawn(distributed_process_ring, ring_node, [N, first_node]),
    debug_print(N, "spawned"),
    Pid;
create(I, N) ->
    Pid = spawn(distributed_process_ring, ring_node, [I, create(I+1, N)]),
    debug_print(I, "spawned"),
    Pid.

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

debug_print(I, Msg) -> io:format("[Process ~p (~p)] ~p~n", [I, self(), Msg]).
