-module(anagrams).
-export([generate/1]).

% Reminder: una stringa è una lista di interi (interi che rappresentano codici carrattere)
%
% Idea per generare gli anagrammi:
%
% [a] | permutazioni(b,c) = permutazioni([a,b,c] -- [a])
%   a,b,c
%   a,c,b
% [b] | permutazioni(a,c) = permutazioni([a,b,c] -- [b])
%   b,a,c
%   b,c,a
% [c] | permutazioni(a,b) = permutazioni([a,b,c] -- [c])
%   c,a,b
%   c,b,a

% Esempio di esecuzione
% > c(anagrams).
% {ok,anagrams}
% > anagrams:generate("abc").
% ["abc","acb","bac","bca","cab","cba"]
% > anagrams:generate("knee").
% ["knee","knee","kene","keen","kene","keen","nkee","nkee",
%  "neke","neek","neke","neek","ekne","eken","enke","enek",
%  "eekn","eenk","ekne","eken","enke","enek","eekn","eenk"]

generate([]) -> [[]];
generate(L)  -> [[H|T] || H <- L, T <- generate(L--[H])].
