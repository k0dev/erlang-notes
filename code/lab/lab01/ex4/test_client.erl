-module(test_client).
-export([test/0]).

test() ->
  echo:start(),
  link(whereis(echo_server)),   % qundo il server termina, terminiamo anche noi
  spawn(fun() -> timer:sleep(5000), echo:stop() end),
  loop(0).

loop(I) ->
  echo:print(I),
  timer:sleep(500),
  loop(I+1).
