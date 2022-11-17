-module(converter).
-export([temperature/0]).

% Esempio di esecuzione dalla shell:
% > c(converter).
% {ok,converter}
% > Pid = spawn(converter, temperature, []).
% <0.89.0>
% > Pid ! {toFahrenheit, 10}.
% 10°C -> 50.0°F
% {toFahrenheit,10}
% > Pid ! {toCelsius, 50}.   
% 50°F -> 10.0°C
% {toCelsius,50}
% > Pid ! {wadawdawd, 50}.
% Unknown message received: {wadawdawd,50}
% {wadawdawd,50}
% > Pid ! {toCelsius, 32}.
% 32°F -> 0.0°C
% {toCelsius,32}
% > Pid ! {stop}.
% Service stopping
% {stop}
% > Pid ! {toCelsius, 32}.
% {toCelsius,32}

% Tranne nel caso in cui vogliamo fermarci ({stop}), richiamiamo temperature() per mantenere attivo il servizio.

temperature() -> 
  receive 
    {toFahrenheit, T} -> io:format("~p°C -> ~p°F~n", [T, T * 9/5 + 32]), temperature();
    {toCelsius, T}    -> io:format("~p°F -> ~p°C~n", [T, (T - 32) * 5/9]), temperature();
    {stop}            -> io:format("Service stopping~n");
    Other             -> io:format("Unknown message received: ~p~n", [Other]), temperature()
  end.
