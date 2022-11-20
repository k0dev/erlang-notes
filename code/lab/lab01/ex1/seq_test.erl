-module(seq_test).
-export([is_palindrome_test/0]).

is_palindrome_test() ->
  Inputs = ["detartrated", "Do geese see God?","Rise to vote, sir.", "random", "aNna", "test"],
  lists:foreach(fun(S) -> 
                    io:format("is ~p palindrome? ~p~n", [S, seq:is_palindrome(S)]) 
                end, 
                Inputs).
