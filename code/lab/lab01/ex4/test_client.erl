-module(test_client).
-export([test/0]).

test() ->
  echo:start(),
  spawn(fun() -> timer:sleep(5000), echo:stop() end),
  loop(0).

loop(I) ->
  echo:print(I),     % crasha dopo la chiamata a echo:stop()
  timer:sleep(500),
  loop(I+1).
