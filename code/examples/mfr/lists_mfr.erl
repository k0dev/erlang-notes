-module(lists_mfr).
-export([map/2, filter/2, reduce/2]).

% Map
map(_, [])                   -> [];
map(Function, [Head | Tail]) -> [Function(Head) | map(Function, Tail)].

% Filter
% (invece di usare un conditional su Predicate(Head), sfruttiamo il pattern matching)
filter(_, [])                       -> [];
filter(Predicate, [Head | Tail])    -> filter(Predicate(Head), Head, Tail, Predicate).
filter(true, Head, List, Predicate) -> [Head | filter(Predicate, List)];
filter(false, _, List, Predicate)   -> filter(Predicate, List).

% Reduce
reduce(_, [])                            -> throw("can't call reduce on an empty list");
reduce(Function, [Head | Tail])          -> reduce(Function, Head, Tail).
reduce(Function, Element, [Head | Tail]) -> reduce(Function, Function(Element, Head), Tail);
reduce(_, Result, [])                    -> Result.
