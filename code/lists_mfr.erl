-module(lists_mfr).
-export([map/2, filter/2, reduce/2]).

map(_, [])                   -> [];
map(Function, [Head | Tail]) -> [Function(Head) | map(Function, Tail)].

filter(_, [])                       -> [];
filter(Predicate, [Head | Tail])    -> filter(Predicate(Head), Head, Tail, Predicate).
filter(true, Head, List, Predicate) -> [Head | filter(Predicate, List)];  % aux function filter/4, not exported
filter(false, _, List, Predicate)   -> filter(Predicate, List).           % aux function filter/4, not exported

reduce(_, [])                            -> throw("can't call reduce on an empty list");
reduce(Function, [Head | Tail])          -> reduce(Function, Head, Tail).
reduce(Function, Element, [Head | Tail]) -> reduce(Function, Function(Element, Head), Tail); % aux function reduce/3, not exported
reduce(_, Result, [])                    -> Result.                                          % aux function reduce/3, not exported
