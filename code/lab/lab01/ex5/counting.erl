-module(counting).
-export([start/0, rpc/2, loop/1]).

% > c(counting).
% {ok,counting}
% > Server = counting:start().
% <0.89.0>
% counting:rpc(Server, dummy1).
% "You called dummy1"
% > counting:rpc(Server, dummy1).
% "You called dummy1"
% > counting:rpc(Server, dummy3).
% "You called dummy3"
% > counting:rpc(Server, dummy3).
% "You called dummy3"
% > counting:rpc(Server, dummy3).
% "You called dummy3"
% > counting:rpc(Server, dummy3).
% "You called dummy3"
% > counting:rpc(Server, tot).   
% [{tot,1},{dummy1,2},{dummy2,0},{dummy3,4}]


% Incrementa di uno il contatore in corrispondenza della chiave specificata
update_count([{S, C}|T], S) -> [{S, C+1}|T];             % matcha se le chiavi (S) sono uguali
update_count([H|T], S)      -> [H | update_count(T, S)].

% Avvia il server e lo inizializza con i contatori a 0 per ogni servizio
start() -> spawn(counting, loop, [[{tot, 0}, {dummy1, 0}, {dummy2, 0}, {dummy3, 0}]]). 

% Wrapper per il client
rpc(Pid, Service) ->
	Pid ! {self(), Service},
	receive
		{Pid, Answer} -> Answer % prendiamo solo i messaggi inviati dal server (match su Pid)
	after(5000) -> exit(timeout)
	end.

% Server loop
loop(L) ->
	receive
		{Pid, tot}    ->
			Updated = update_count(L, tot),
			Pid ! {self(), Updated},
			loop(Updated);
		{Pid, dummy1} -> dummy1(Pid), loop(update_count(L, dummy1));
		{Pid, dummy2} -> dummy2(Pid), loop(update_count(L, dummy2));
		{Pid, dummy3} -> dummy3(Pid), loop(update_count(L, dummy3))
	end.

% Servizi offerti dal server
dummy1(Pid) -> Pid ! {self(), "You called dummy1"}.
dummy2(Pid) -> Pid ! {self(), "You called dummy2"}.
dummy3(Pid) -> Pid ! {self(), "You called dummy3"}.

