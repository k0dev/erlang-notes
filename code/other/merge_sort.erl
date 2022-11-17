-module(merge_sort).
-export([merge_sort/1, merge_sort/2]).

% implementazione naive, spawna un processo per ogni sublist (anche se la lista da ordinare è molto corta), quindi crasha per liste grandi

merge([], [], C, _) -> lists:reverse(C);
merge([], [B|BS], C, P) ->
	merge([], BS, [B|C], P);
merge([A|AS], [], C, P) -> 
	merge(AS, [], [A|C], P);
merge([A|AS], [B|BS], C, Pred) -> 
	Bool = Pred(A, B),
	if Bool -> merge(AS, [B|BS], [A|C], Pred);
	   true -> merge([A|AS], BS, [B|C], Pred)
	end.

impera(Hd, Tl, true, true, Father, Pred) ->
	Father ! {ok, merge(Hd, Tl, [], Pred)};
impera(Hd, Tl, GotHd, GotTl, Father, Pred) ->
	receive
		{hd, L} -> impera(L, Tl, true, GotTl, Father, Pred);
		{tl, L} -> impera(Hd, L, GotHd, true, Father, Pred);
		_       -> impera(Hd, Tl, GotHd, GotTl, Father, Pred)
	after (length(Hd) + length(Tl)) * 100 + 10000 -> exit(timeout)
	end.

divide([L], _, Merger, Pos) -> 
	Merger ! {Pos, [L]};
divide([A, B], Pred, Merger, Pos) ->
	Bool = Pred(A, B),
	L = if Bool -> [A, B];
	       true -> [B, A]
	    end,
	Merger ! {Pos, L};
divide(List, Pred, Merger, Pos) ->
	Len = length(List) div 2,
	{Hd, Tl} = lists:split(Len, List),
	Pid = self(),
	Aggregator = spawn_link(fun () -> impera([], [], false, false, Pid, Pred) end),
	spawn_link(fun () -> divide(Hd, Pred, Aggregator, hd) end),
	spawn_link(fun () -> divide(Tl, Pred, Aggregator, tl) end),
	receive
		{ok, Merged} -> Merger ! {Pos, Merged};
		_            -> exit(undefined)
	after length(List) * 100 + 10000 -> exit(timeout)
	end.

merge_sort(List, Pred) -> 
	Pid = self(),
	spawn_link(fun () -> divide(List, Pred, Pid, hd) end),
	receive
		{hd, Merged} -> Merged;
		_            -> exit(undefined)
	after length(List) * 100 + 10000 -> exit(timeout)
	end.

merge_sort(A) -> merge_sort(A, fun (X, Y) -> X < Y end).


