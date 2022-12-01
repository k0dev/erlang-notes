-module(die_togheter).
-export([example1/0, example2/0, example3/0, dummy/2, dummy/1]).

%% Situazione di partenza in ogni esempio %%
%                                          %
%            P2    P1   P4                 %
%          /  \                            %
%         P6  P3                           %
%              \                           %
%              P5                          %
%                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setup() ->
    P1 = spawn(?MODULE, dummy, [p1]),
    P2 = spawn(?MODULE, dummy, [p2]),
    P3 = spawn(?MODULE, dummy, [p3, P2]),
    P4 = spawn(?MODULE, dummy, [p4]),
    P5 = spawn(?MODULE, dummy, [p5, P3]),
    P6 = spawn(?MODULE, dummy, [p6, P2]),
    [{p1, P1}, {p2, P2}, {p3, P3}, {p4, P4}, {p5, P5}, {p6, P6}].


% Invia un messaggio di stop a tutti i processi nella lista
clear([{_, Pid}])   -> Pid ! stop;
clear([{_, Pid}|T]) -> Pid ! stop, clear(T).


% Simulo un crash nel processo P3, quindi termineranno
% anche tutti gli altri processi nel suo linkset
example1() ->
    Processes = setup(),
    pp_print_alive(Processes),                       % tutti i processi sono vivi
    exit(proplists:get_value(p3, Processes), crash), % simulo un crash in p3
    timer:sleep(50),                                 % aspetto che i segnali di uscita si propaghino
    pp_print_alive(Processes),                       % rimangono solo i processi P1 e P4
    clear(Processes).                                % termino anche P1 e P4


% Simulo un crash nel processo P1, quindi terminerà solo
% lui in quanto il suo linkset è vuoto.
example2() ->
    Processes = setup(),
    pp_print_alive(Processes),                       % tutti i processi sono vivi
    exit(proplists:get_value(p1, Processes), crash), % simulo un crash in p1
    timer:sleep(50),                                 % aspetto che i segnali di uscita si propaghino
    pp_print_alive(Processes),                       % è crashato solo p1 (il suo linkset è vuoto)
    clear(Processes).                                % termino tutti


% Faccio terminare normalmente il processo P3. Terminerà
% solo lui (nonostante il suo linkset non sia vuoto) perché
% appunto la terminazione è normale, senza anomalie
example3() ->
    Processes = setup(),
    pp_print_alive(Processes),                       % tutti i processi sono vivi
    proplists:get_value(p3, Processes) ! stop,       % faccio terminare normalmente P3
    timer:sleep(50),                                 % aspetto che i segnali di uscita si propaghino
    pp_print_alive(Processes),                       % è terminato solo P3
    clear(Processes).                                % termino tutti

% Utility functions

pp_print_alive([]) ->
    io:format("---------------------------~n");
pp_print_alive([{Name, Pid}|T]) ->
    io:format("Is ~p at ~p alive? ~p~n", [Name, Pid, is_process_alive(Pid)]),
    pp_print_alive(T).

dummy(Name, Pid) ->
    link(Pid),
    receive
        stop -> io:format("I (~p at ~p) received a stop message and terminated normally~n", [Name, self()])
    end.

dummy(Name) ->
    receive
        stop -> io:format("I (~p at ~p) received a stop message and terminated normally~n", [Name, self()])
    end.