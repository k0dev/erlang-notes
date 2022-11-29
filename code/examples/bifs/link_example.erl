-module(link_example).
-export([example/0, p1/0, p2/0]).

% Esempio di esecuzione:
%
% > link_example:example().
% Is P1 alive? true
% [<0.159.0>] I will crash soon
% [<0.160.0>] I am alive
% [<0.160.0>] I am alive
% [<0.160.0>] I am alive
% [<0.160.0>] I am alive
% [<0.160.0>] I am alive
% [<0.159.0>] Crashing now
% =ERROR REPORT==== 29-Nov-2022::11:49:08.904585 ===
% Error in process <0.159.0> with exit value:
% {badarith,[{link_example,p2,0,[{file,"link_example.erl"},{line,28}]}]}
%
% Is P1 alive? false
% ok

example() ->
    P2 = spawn(link_example, p2, []),        % spawno P2 (il processo che crasha dopo 5 secondi)
    P1 = spawn(fun() -> link(P2), p1() end), % spawno P1 (il processo linkato a P2)
    io:format("Is P1 alive? ~p~n", [is_process_alive(P1)]),
    receive
    after(10000) -> void                     % aspetto 10 secondi
    end,
    % P1 è terminato nonostante fosse in un loop infinito (perchè era collegato ad un processo che è crashato)
    io:format("Is P1 alive? ~p~n", [is_process_alive(P1)]).

% loop infinito, non dovrebbe mai terminare (a meno che un processo collegato ad esso non termini con un anomalia)
p1() ->
    io:format("[~p] I am alive~n", [self()]),
    % Sleep di un secondo
    receive
    after(1000) -> void
    end,
    p1().

% crasha dopo 5 secondi
p2() -> 
    io:format("[~p] I will crash soon~n", [self()]),
    receive
    after(5000) -> io:format("[~p] Crashing now~n", [self()]), 
                   10/0 % (Warning: evaluation of operator '/'/2 will fail with a 'badarith' exception)
    end.
