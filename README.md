# Erlang :(

- Erlang è orientato alla concorrenza, ovvero il processo è la base di ogni computazione.
- Dinamically typed functional language
- It supports distribution, fault tolerance and hot-swapping


## Actor model
- Scambio di messaggi asincrono
- Memoria **non** condivisa1

## Primo programma
```erlang
-module(factorial).
-export([factorial/1]).

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).
```
```erlang
> c(factorial).
{ok,factorial}
> factorial:factorial(7).
5040
> factorial:factorial(100).
93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
```

## Numeri
- E' possibile specificare la base del numero con la seguente sintassi: base#num
- E' possibile usare la notazione scientifica
- Si può ottenere il char code di un carattere con $char

<!--- TODO: float -->
```erlang
> 10.
10
> -22.
-22
> 16#FF.
255
> 2#101.
5
> 17#G.
16
> $A.
65
> $G.
71
> -12.35e-2.
-0.1235
```

## Atomi
Gli atomi devono iniziare con una lettera minuscola. Se racchiusi tra apici possono iniziare con la lettera maiuscola e contenere caratteri speciali.
```erlang
> test_@atomo.
test_@atomo
> 'test & atomo'.
'test & test'
> 'Atomo'.      
'Atomo'
```

## Tuple
Eterogenee, possono essere nidificate, ecc.

Sono tuple :D
```erlang
> {"a", "b"}.
{"a","b"}
> {}.
{}
> {colore, {120, 255, 80}}.
{colore,{120,255,80}}
> {{1,2},3}=={1,{2,3}}.
false
```

## Liste
Anche loro sono eterogenee.
```erlang
> [].
[]
> [1 | []].
[1]
> [1 | [2]].
[1,2]
> [1 | [2 | [3]]].
[1,2,3]
> [1, 2, 3].
[1,2,3]
> [1, "due", 3, ok].
[1,"due",3,ok]
```

Le stringhe sono liste di caratteri (evviva)
```erlang
> [$T, $e, $s, $t].
"Test"
```
Concatenazione di stringhe
```erlang
> "Uno "++"due".
"Uno due"
> [$T, $e, $s, $t]++" "++[$1, $2, $3].
"Test 123"
```
Sottrazione tra stringhe
```erlang
> "AAaabbcc"--"acbA".
"Aabc"
> "AAaabbcc"--"acbAc".
"Aab"
```

## Assegnamento 
L'operazione di *assegnamento* binda un nome ad un valore. Una volta assegnato, non si può modificare.
Le "variabili" devono iniziare con una lettera maiuscola.
_ è anonima.
```erlang
> A = 1.
1
> B = 2.
2
> A = 4.
** exception error: no match of right hand side value 4
> _ = 4.
4
> _ = 9.
9
```
I binding sono creati mediante pattern matching.
```erlang
> [Hd|Tl] = [1,2,3,4,5].
[1,2,3,4,5]
> Hd.
1
> Tl.
[2,3,4,5]
> {X, _, Y} = {10, 20, 30}.
{10,20,30}
> X.
10
> Y.
30
```
## Funzioni
Vengono analizzate sequenzialmente fino a che una non effettua il match. 
```erlang
name(pattern11 , pattern12 , ..., pattern1n) [when guard1 ] -> body1;
name(pattern21 , pattern22 , ..., pattern2n) [when guard2 ] -> body2;
...
name(patternk1 , patternk2 , ..., patternkn) [when guardk ] -> bodyk.
```

Esempio:

https://github.com/k0dev/erlang-notes/blob/62cfdbaaf968544d02530af28cd4a3564b3eaf74/code/function_example.erl#L1-L8

## Moduli
```erlang
-module(mio_modulo).
-export([mia_funzione/3]).
```
Significa che il modulo `mio_modulo` esporta una funzione chiamata `mia_funzione` la quale accetta 3 parametri. Esportare una funzione significa renderla disponibile ad altri moduli.

<!--- TODO:  guard sequence -->

## Map, Filter, Reduce
https://github.com/k0dev/erlang-notes/blob/2c42d3d9e40f6f1597025c93c2b0890d66596de0/code/lists_mfr.erl#L1-L15