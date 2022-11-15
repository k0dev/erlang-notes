-module(primes).
-export([untill/1]).

% Esempio di esecuzione dalla shell:
% > c(primes).         
% {ok,primes}
% > primes:untill(100).
% [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
% > primes:untill(2).
% [2]
% > primes:untill(3).
% [2, 3]
% > primes:untill(1).
% []
% > primes:untill(-10).
% []

% utilizziamo lists:seq/2 per generare una sequenza di interi in un dato range (estremi inclusi).
% lists:seq/2 solleva un eccezione quando To < From-1.
% Quindi lists:seq(2, trunc(math:sqrt(N))) non da mai problemi se N>2, infatti:
%   N=2 ===>  math:sqrt(2)~=1.414  ===>  trunc(math:sqrt(N)) = 1  ===>  From=2 e To=1 ===> !(1 < 2-1)

untill(N) when N<2  -> [];
untill(N)           -> [X || X<-lists:seq(2, N), is_prime(X)].

is_prime(N) -> length([X || X<-lists:seq(2, trunc(math:sqrt(N))), N rem X == 0]) == 0.
