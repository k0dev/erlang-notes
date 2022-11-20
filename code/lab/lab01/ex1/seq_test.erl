-module(seq_test).
-export([is_palindrome_test/0, is_an_anagram_test/0]).

is_palindrome_test() ->
  Inputs = ["detartrated", "Do geese see God?","Rise to vote, sir.", "random", "aNna", "test"],
  lists:foreach(fun(S) -> 
                    io:format("is ~p palindrome? ~p~n", [S, seq:is_palindrome(S)]) 
                end, 
                Inputs).

is_an_anagram_test() ->
  Dict = ["jioawd", "oawijaw", "iwad", "anna", "knee"],
  Inputs = ["nana", "keen", "ciao", "test"],
  lists:foreach(fun(S) ->
                    io:format("is ~p an anagram? ~p~n", [S, seq:is_an_anagram(S, Dict)])
                end,
                Inputs).
