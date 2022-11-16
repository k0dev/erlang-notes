# Erlang :(

- Erlang è orientato alla concorrenza, ovvero il processo è la base di ogni computazione.
- Dinamically typed functional language
- It supports distribution, fault tolerance and hot-swapping


## Actor model
Ogni oggetto è un attore (*actor*), con un comportamento definito e una mailbox. Gli attori comunicano tra di loro tramite le mailbox (quindi si scambiano messaggi). Ogni attore è implementato come un thread *leggero* a livello utente, pertanto è caratterizzato da un indirizzo univoco (pid).

Quando un attore riceve un messaggio:
- può inviare altri messaggi a sua volta
- può creare altri attori
- può eseguire un azione
- può trattare i messaggi successivi in modo differente

In generale: 
- Lo scambio di messaggi avviene in modo asincrono
  - il mittente non aspetta che il messaggio venga ricevuto dopo averlo inviato
  - non ci sono garanzie sull'ordine di ricevimento dei messaggi
- Memoria **non** condivisa tra diversi attori
  - le informazioni sullo stato sono richieste e inviate solo tramite messaggi
  - la manipolazione dello stato avviene tramite scambio di messaggi

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

## List Comprehensions 
[Reference](https://www.erlang.org/doc/reference_manual/expressions.html#list-comprehensions)

Forniscono una notazione succinta per la generazione di elementi in una lista.
Sintassi:
```
[Expr || Qualifier1,...,QualifierN]
```
- `Expr` è un'espressione qualsiasi
- `Qualifier` può essere un generatore o un filtro

Un generatore si scrive come `Pattern <- ListExpr`, dove `ListExpr` deve essere un'espressione che viene valutata come lista.

Un filtro può essere un' espressione che deve valere true o false, oppure deve essere una *guard expression*.

Le variabili nei pattern dei generatori (`Pattern`) oscurano le variabili precedentemente assegnate.

Una list comprehension restituisce quindi una lista dove gli elementi sono il risultato della valutazione di `Expr` per ogni combinazione degli elementi dei generatori per i quali tutti i filtri sono `true`.

```erlang
> L1=[1,2].
[1,2]
> L2=["a","b"].
["a","b"]
> [{X,Y} || X<-L1, Y<-L2]. % per ogni combinazione ~= prodotto cartesiano
[{1,"a"},{1,"b"},{2,"a"},{2,"b"}]
> [X || X<-L1, Y<-L2].
[1,1,2,2]
> [X || X<-L1, X<-L2]. % "prevale" la X più a destra
["a","b","a","b"]
> [X*2 || X <- [1,2,3,4,5,6,7], X>3]. % raddoppiamo tutti gli elementi maggiori di 3
[8,10,12,14]
```
Esempi:
- [quick sort](code/list_comprehension/qsort.erl)
- [numeri primi <= n](code/list_comprehension/primes.erl)

## Concorrenza: introduzione
Erlang mette a disposizione tre funzionalità di base per realizzare la concorrenza:
- la funzione built-in (BIF - built-in function) `spawn` per creare nuovi actors
- l'operatore `!` per inviare un messaggio ad un actor
- un meccanismo per eseguire il pattern matching sui messaggi nella mailbox

```erlang
-module(concurrency_test).
-export([start/2, loop/2]).

% spawn(module_name, function, param_list)
start(N, Id) -> spawn(concurrency_test, loop, [N, Id]).

% self() restituisce il pid del thread corrente
loop(0, Id) -> io:format("(pid: ~p - id: ~p) end~n", [self(), Id]);
loop(N, Id) -> io:format("(pid: ~p - id: ~p) ~p~n", [self(), Id, N]), loop(N-1, Id).
```

1. `concurrency_test:loop(10, a), concurrency_test:loop(10, b).`
2. `concurrency_test:start(10, a), concurrency_test:start(10, b).`

La differenza è che nel primo modo eseguiamo due conti alla rovescia in modo sequenziale, nello stesso thread/actor. Nel secondo modo invece creiamo un actor per ogni conto alla rovescia, quindi i conteggi saranno eseguiti in maniera concorrente.

## Invio di messaggi
Per poter inviare un messaggio tramite la primitiva `!`, un actor deve conoscere il pid del destinatario. Nel caso in cui sia necessario ricevere una risposta, il mittente deve includere nel messaggio anche il suo pid. La sintassi è la seguente:
```
Exp1 ! Exp2
```
dove `Exp1` deve identificare l'attore destinatario e `Exp2` può essere qualsiasi espressione valida. Il risultato dell'espressione send (`!`) è il valore di `Exp2`.

L'invio non fallisce mai, nemmeno quando il pid specificato non appartiene ad alcun actor. Inoltre l'operazione di send non è bloccante per il mittente.

## Ricezione di messaggi
La ricezione dei messaggi avviene mediante pattern matching:
```erlang
receive
  Pattern_1 [when GuardSeq_1] -> Body_1 ;
  ...
  Pattern_n [when GuardSeq_n] -> Body_n
  [after Expr_t -> Body_t]
end
```
L'actor tenta di prendere dalla mailbox il messaggio più vecchio che effettua il match con uno dei pattern. Se nessun messaggio effettua il match, l'actor aspetta indefinitamente in attesa che arrivi un messaggio valido. Se la clausola after è specificata, l'actor aspetta per un tempo massimo di `Expr_t` millisecondi, per poi valutare il contenuto di `Body_t`.
```erlang
receive
  Any -> do_something(Any)
end
```
In questo esempio l'actor prenderà qualsiasi messaggio, quindi rimane in attesa solo quando la mailbox è vuota.
```erlang
receive
  {Pid, something} -> do_something(Pid)
end
```
In questo esempio l'actor prende (se esiste) il messaggio più vecchio che effettua il match con `{Pid, something`}. Rimane in attesa fintanto che la mailbox è vuota o non contiene messaggi di questo tipo.