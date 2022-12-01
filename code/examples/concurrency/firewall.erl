-module(firewall).
-export([example1/0, example2/0, example3/0, dummy/1, dummy/2, dummy/3]).

%% Situazione di partenza in ogni esempio %%
%                                          %
%            P1     P2  P7                 %
%             \                            %
%             [P3] (system process)        %
%               \                          %
%               P4                         %
%                \                         %
%                P5 ― P6                   %
%                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setup() ->
    P1 = spawn(?MODULE, dummy, [p1]),
    P2 = spawn(?MODULE, dummy, [p2]),
    P3 = spawn(?MODULE, dummy, [p3, P1, system]),
    P4 = spawn(?MODULE, dummy, [p4, P3]),
    P5 = spawn(?MODULE, dummy, [p5, P4]),
    P6 = spawn(?MODULE, dummy, [p6, P5]),
    P7 = spawn(?MODULE, dummy, [p7]),
    [{p1, P1}, {p2, P2}, {p3, P3}, {p4, P4}, {p5, P5}, {p6, P6}, {p7, P7}].


% Invia un messaggio di stop a tutti i processi nella lista
clear([{_, Pid}])   -> Pid ! stop;
clear([{_, Pid}|T]) -> Pid ! stop, clear(T).


% Simulo un crash nel processo P5 che il firewall 
% non propagherà. Termineranno quindi tutti i processi
% collegati fino al firewall.
example1() ->
    Processes = setup(),                                    
    pp_print_alive(Processes),                              % tutti i processi sono vivi
    exit(proplists:get_value(p5, Processes), simple_crash), % termino P5 con motivazione simple_crash
    timer:sleep(50),                                        % aspetto che i segnali di uscita si propaghino
    pp_print_alive(Processes),                              % sono terminati P4, P5, P6 (ma non P1 e P3)
    clear(Processes).                                       % termino tutti

% Simulo un crash nel processo P5 che il firewall
% propagherà. Termineranno quindi tutti i processi
% collegati (incluso il firewall).
example2() ->
    Processes = setup(),                                    
    pp_print_alive(Processes),                              % tutti i processi sono vivi
    exit(proplists:get_value(p5, Processes), unexpected),   % termino P5 con motivazione unexpected
    timer:sleep(50),                                        % aspetto che i segnali di uscita si propaghino
    pp_print_alive(Processes),                              % sono terminati P4, P5, P6, e anche P1 e P3
    clear(Processes).                                       % termino tutti

% Termino normalmente P4. Nessun altro processo
% terminerà.
example3() ->
    Processes = setup(),                                    
    pp_print_alive(Processes),                              % tutti i processi sono vivi
    proplists:get_value(p4, Processes) ! stop,              % faccio terminare p4 normalmente
    timer:sleep(50),                                        % aspetto che i segnali di uscita si propaghino
    pp_print_alive(Processes),                              % è terminato solo p4
    clear(Processes).                                       % termino tutti

% Utility functions

pp_print_alive([]) ->
    io:format("---------------------------~n");
pp_print_alive([{Name, Pid}|T]) ->
    io:format("Is ~p at ~p alive? ~p~n", [Name, Pid, is_process_alive(Pid)]),
    pp_print_alive(T).

dummy(Name, Pid, system) ->
    process_flag(trap_exit, true),
    link(Pid),
    receive
        {'EXIT', From, simple_crash} -> 
            io:format("I (firewall ~p at ~p) blocked an exit signal from ~p because it was a simple crash~n", [Name, self(), From]),
            % <code to handle the crash>
            dummy(Name, Pid, system);
        {'EXIT', From, normal} -> 
            io:format("I (firewall ~p at ~p) just received a normal exit signal from ~p~n", [Name, self(), From]),
            dummy(Name, Pid, system);
        {'EXIT', From, Why} ->
            io:format("I (firewall ~p at ~p) received an unknown exit reason from ~p. I will crash.~n", [Name, self(), From]),    
            exit(Why);
        stop -> io:format("I (~p at ~p) received a stop message and terminated normally~n", [Name, self()])
    end.

dummy(Name, Pid) ->
    link(Pid),
    dummy(Name).

dummy(Name) ->
    receive
        stop -> io:format("I (~p at ~p) received a stop message and terminated normally~n", [Name, self()])
    end.