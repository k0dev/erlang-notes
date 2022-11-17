-module(areas).
-export([loop/0]).

loop() -> 
  receive
    {rectangle, W, H} -> io:format("L'area del rettangolo specificato è ~p~n", [W*H]),
                         loop();
    {circle, R}       -> io:format("L'area del cerchio specificato è ~p~n", [math:pi() * R * R]),
                         loop();
    stop              -> io:format("Fine.~n");
    Other             -> io:format("Errore: ~p~n", [Other]),
                         loop()
  end.
