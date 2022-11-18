-module(benchmark).
-export([test_sleep/1, test_list_rev/1]).

% Reference: https://www.erlang.org/doc/efficiency_guide/profiling.html#benchmarking

% Esempio di esecuzione:
%
% > c(benchmark).
% {ok,benchmark}
% > spawn(benchmark, test_sleep, [3000]).
% <0.89.0>
% Sono passati 3001ms, cpu time: 5ms
% > spawn(benchmark, test_sleep, [3000]).
% <0.91.0>
% Sono passati 3001ms, cpu time: 1ms
% > spawn(benchmark, test_list_rev, [10000]).
% Sono passati 1ms, cpu time: 0ms
% <0.93.0>
% > spawn(benchmark, test_list_rev, [1000000]).
% <0.95.0>
% Sono passati 20ms, cpu time: 17ms
% > spawn(benchmark, test_list_rev, [100000000]).
% <0.97.0>
% Sono passati 10557ms, cpu time: 1961ms


test_sleep(Ms) -> 
  statistics(wall_clock), statistics(runtime),
  timer:sleep(Ms),
  {_, ElapsedWC} = statistics(wall_clock), {_, CpuTime} = statistics(runtime),
  io:format("Sono passati ~pms, cpu time: ~pms~n", [ElapsedWC, CpuTime]).

test_list_rev(Len) -> 
  L = lists:seq(1, Len),
  statistics(wall_clock), statistics(runtime), % non voglio includere nel conteggio il tempo usato per generare la lista
  lists:reverse(L),
  {_, ElapsedWC} = statistics(wall_clock), {_, CpuTime} = statistics(runtime),
  io:format("Sono passati ~pms, cpu time: ~pms~n", [ElapsedWC, CpuTime]).
