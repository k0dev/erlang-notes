-module(forloop).
-export([for/2, for/3]).

% Esempio di esecuzione:
%
% > c(forloop).
% {ok,forloop}
% > forloop:for(fun(X) -> X end, 10).
% [0,1,2,3,4,5,6,7,8,9,10]
% > forloop:for(fun(X) -> X end, -10).
% [0,-1,-2,-3,-4,-5,-6,-7,-8,-9,-10]
% > forloop:for(fun(X) -> X end, 5, 10).
% [5,6,7,8,9,10]
% > forloop:for(fun(X) -> X end, 10, 5).
% [10,9,8,7,6,5]
% > forloop:for(fun(X) -> X end, 10, 10).
% "\n"
% > forloop:for(fun(X) -> X end, 1, 1).
% [1]
% > forloop:for(fun(X) -> X*X end, 0, 20).
% [0,1,4,9,16,25,36,49,64,81,100,121,144,169,196,225,256,289,324,361,400]

%% funzioni esportate %%

% con le guard verifico se l'incremento deve essere positivo o negativo
for(F, Max) when Max < 0 -> for(F, 0, Max, -1);
for(F, Max)              -> for(F, 0, Max, +1).

for(F, Min, Max) when Min > Max  -> for(F, Min, Max, -1);
for(F, Min, Max)                 -> for(F, Min, Max, +1).


%% funzioni ausiliarie %%

% non utilizzo la ricorsione in coda perché:
%   (https://www.erlang.org/doc/efficiency_guide/myths.html)
%   "A body-recursive function generally uses the same amount of memory as 
%   a tail-recursive function. It is generally not possible to predict 
%   whether the tail-recursive or the body-recursive version will be 
%   faster. Therefore, use the version that makes your code cleaner (hint: 
%   it is usually the body-recursive version)."

for(F, Max, Max, _) -> [F(Max)]; % fa il match quando il secondo e il terzo parametro sono uguali
for(F, I, Max, Inc) -> [F(I) | for(F, I+Inc, Max, Inc)].
