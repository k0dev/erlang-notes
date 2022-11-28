-module(counting).
-export([start/0, rpc/2, loop/1]).

-define(zero(I), {I, 0}).

update_count([{S, C}|T], S) -> [{S, C+1}|T];
update_count([H|T], S)      -> [H | update_count(T, S)].

start() -> spawn(counting, loop, [[?zero(tot), ?zero(dummy1), ?zero(dummy2), ?zero(dummy3)]]).


rpc(Pid, Service) ->
	Pid ! {self(), Service},
	receive
		Msg -> Msg
	after(5000) -> exit("RPC failed")
	end.


loop(L) ->
	receive
		{Pid, tot}    -> 
			Updated = update_count(L, tot),
			Pid ! Updated,
			loop(Updated);
		{Pid, dummy1} -> dummy1(Pid), loop(update_count(L, dummy1));
		{Pid, dummy2} -> dummy2(Pid), loop(update_count(L, dummy2));
		{Pid, dummy3} -> dummy3(Pid), loop(update_count(L, dummy3))
	end.


dummy1(Pid) -> Pid ! "You called dummy1".
dummy2(Pid) -> Pid ! "You called dummy2".
dummy3(Pid) -> Pid ! "You called dummy3".

