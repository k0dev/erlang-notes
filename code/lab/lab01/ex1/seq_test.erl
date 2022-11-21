-module(seq_test).
-export([is_palindrome_test/0, is_an_anagram_test/0, factors_test/0, is_proper_test/0]).

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

factors_test() ->
  Inputs = [332, 2, 10, 100],
  lists:foreach(fun(N) ->
                    io:format("~p factors: ~p~n", [N, seq:factors(N)])
                end,
                Inputs).

is_proper_test() ->
  Inputs = [2, 6, 13, 28, 200, 496, 8128],
  lists:foreach(fun(N) ->
                    io:format("is ~p a perfect number? ~p~n", [N, seq:is_proper(N)])
                end,
                Inputs).
