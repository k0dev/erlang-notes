# Erlang :(

<!--toc:start-->
- [Shell cheat sheet](#shell-cheat-sheet)
- [Actor model](#actor-model)
- [Primo programma](#primo-programma)
- [Numeri](#numeri)
- [Atomi](#atomi)
- [Tuple](#tuple)
- [Liste](#liste)
- [Stringhe](#stringhe)
- [Assegnamento](#assegnamento)
- [Funzioni](#funzioni)
- [Funzioni anonime](#funzioni-anonime)
- [Moduli](#moduli)
- [Map, Filter, Reduce](#map-filter-reduce)
- [List Comprehensions](#list-comprehensions)
- [Concorrenza: introduzione](#concorrenza-introduzione)
- [Invio di messaggi](#invio-di-messaggi)
- [Ricezione di messaggi](#ricezione-di-messaggi)
- [Actors registrati](#actors-registrati)
<!--toc:end-->

- Erlang è orientato alla concorrenza, ovvero il processo è la base di ogni computazione.
- Dinamically typed functional language
- It supports distribution, fault tolerance and hot-swapping

## Shell cheat sheet
Questi comandi funzionano **solo** nella shell, **non** possono essere utilizzati nei file sorgente.
- `help().` -> mostra lista comandi
- `q().`    -> shutdown controllato (tutti i file aperti vengono flushati e chiusi, i database aperti vengono fermati, ecc.)
- `halt()`  -> shutdown immediato
- `f()`     -> unbound di tutte le variabili
- `f(X)`    -> unbound della variabile X
- `c(Mod)`  -> compila e carica il modulo Mod
- `pwd()`   -> stampa il path della cartella corrente
- `ls()`    -> elenca i nomi dei file nella cartella corrente
- `cd(Dir)` -> cambia la cartella corrente in Dir

<!-- TODO: common problems: can’t load a module that resides in a sticky directory, liste di numeri stampate come stringe, eseguire il compilatore dalla stessa dir del file (anche quando si compila dalla shell con c(Mod), ecc. -->

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
% Nel file factorial.erl
-module(factorial).
-export([factorial/1]).

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).
```
```erlang
% Nella shell di erlang
> c(factorial).
{ok,factorial}
> factorial:factorial(7).
5040
> factorial:factorial(100).
93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
```

## Numeri
(*le parentesi angolari servono solo per identificare una label, non vanno inserite!
`<base>` &rarr; `base`*)
- E' possibile specificare la base del numero con la seguente sintassi: `<base>#<num>`
- E' possibile usare la notazione scientifica
- Si può ottenere il char code di un carattere con `$<char>`

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
> 0.7.
0.7
```

## Atomi
Gli atomi devono iniziare con una lettera minuscola e possono contenere l'underscore `_` o la chiocciola `@`. Se racchiusi tra apici possono iniziare con la lettera maiuscola e contenere caratteri speciali.

Gli atomi sono globali e vengono usati per rappresentare valori costanti.

Il valore di un atomo è l'atomo stesso.
```erlang
> test_@atomo.
test_@atomo
> 'test & atomo'.
'test & test'
> 'Atomo'.      
'Atomo'
```

## Tuple
Possiamo creare una tupla racchiudendo tra graffe alcuni valori, separandoli con la virgola: `{val1, val2, val3}`

I campi delle tuple non hanno un nome. E' una buona idea mettere un atomo come primo elemento della tupla, in modo da descriverne il contenuto.
```erlang
{255, 10, 20}
{rgb, 255, 10, 20} % più chiaro
```
Le tuple in Erlang sono eterogenee.
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
Possiamo creare tuple nidificate:
```erlang
> {color, {red, 255}, {green, 10}, {red, 50}}. % usiamo gli atomi per identificare sia il contenuto generale, sia il contenuto dei vari "campi"
{color,{red,255},{green,10},{red,50}}
```
Per estrarre i valori dalle tuple usiamo il pattern matching:
```erlang
> C = {color, 255, 10, 20}.
{color,255,10,20}
> {color, R, G, B} = C.
{color,255,10,20}
> R.
255
> G.
10
> B.
20
```

## Liste
Possiamo creare una lista racchiudendo tra quadre alcuni valori, separandoli con la virgola: `[val1, val2, val3]`.
```erlang
> []. % lista vuota
[]
> [1, 2, 3].
[1,2,3]
> [{rgb, 255, 10, 20}, {rgb, 100, 50, 30}].
[{rgb,255,10,20},{rgb,100,50,30}]
```
Come le tuple, anche le liste sono eterogenee:
```erlang
> [1, "due", 3.0, atomo].
[1,"due",3.0,atomo]
```
Se `TL` è una lista, allora `[HD | TL]` è una lista con testa `HD` e coda `TL`. La pipe `|` separa quindi la testa dalla coda.
E' possibile aggiungere più di un elemento all'inizio di `TL` con la seguente sintassi: `[E1, ..., En | TL]`.
```erlang
> [1 | []].
[1]
> [1 | [2]].
[1,2]
> [1 | [2 | [3]]].
[1,2,3]
> [1, 2, 3 | [4, 5, 6]].
[1,2,3,4,5,6]
```
Per estrarre i valori dalle liste usiamo il pattern matching. Siano `H` e `T` delle variabii unbounded. Se `L` è una lista non vuota, allora `[H | T] = L` assegna ad `H` la testa della lista e a `T` la coda.
```erlang
> L = [1, 2, 3, 4].
[1,2,3,4]
> [H | T] = L.
[1,2,3,4]
> H.
1
> T.
[2,3,4]
```
```erlang
> [First, Second, Third | Rest] = [1,2,3,4,5,6,7].
[1,2,3,4,5,6,7]
> First.
1
> Second.
2
> Third.
3
> Rest.
[4,5,6,7]
> [1, 2, 3, 1, 2, 3] -- [2, 2, 1, 3]. % sottrazione tra liste
[1,3]
> [9, 8, 7] ++ [1, 2, 3]. % concatenazione tra liste
[9,8,7,1,2,3]
```

## Stringhe
Erlang non mette a disposizione un tipo stringa, le tratta invece come liste di codici carattere (quindi in genere le operazioni eseguibili sulle liste si possono effettuare anche sulle stringhe).
```erlang
> [$T, $e, $s, $t].
"Test"
```
Ovviamente sarebbe scomodo utilizzare questa notazione, quindi abbiamo comunque la possibilità di usare i letterali stringa che verranno convertiti automaticamente in liste di codici caratteri.
```erlang
> "Test".
"Test"
> "Test" == [$T, $e, $s, $t].
true
> [H | T] = "Test".
"Test"
> H. % contiene il codice del carattere 'T'
84
> T.
"est"
```
⚠️  **Attenzione**: dato che una stringa è di fatto rappresentata come una lista di interi, la shell potrebbe stampare le liste di interi in modo inaspettato:
```erlang
> [75, 101, 118, 105, 110]. % sono tutti codici di caratteri stampabili       
"Kevin"
> [1, 75, 101, 118, 105, 110]. % 1 non è il codice di un carattere stampabile
[1,75,101,118,105,110]
> io:format("~p~n",["abc"]). % stampa come stringa
"abc"
ok
> io:format("~w~n",["abc"]). % stampa come lista di interi
[97,98,99]
ok
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
`_` è anonima.
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

## Funzioni anonime
```erlang
> Next = fun(K) -> K+1 end.
#Fun<erl_eval.42.3316493>
> Next(3).
4
> Next(5).
6
> Invert = fun(true) -> false; (false) -> true end.
#Fun<erl_eval.42.3316493>
> Invert(true).
false
> Invert(false).
true
```
Esempi:
- [for loop](code/examples/lambdas/forloop.erl)

## Moduli
I moduli contengono funzioni, le quali possono essere eseguite sequenzialmente o in parallelo.

I moduli sono l'unità base di Erlang. Sono contenuti in file `.erl` e vengono compilati in `.beam`.

La prima riga di un file è un *module declaration*, e il nome del modulo nella declaration deve essere lo stesso del file senza estensione. Quindi ad esempio nel file `mio_modulo.erl` possiamo avere:
```erlang
-module(mio_modulo).
-export([mia_funzione/3]).
```
Questo significa che il modulo `mio_modulo` esporta una funzione chiamata `mia_funzione` la quale accetta 3 parametri. Esportare una funzione significa renderla disponibile ad altri moduli. Le funziono non esportate non possono essere chiamate da altri moduli.

<!--- TODO:  guard sequence -->

## Map, Filter, Reduce
```erlang
% utilizzando le funzioni dal modulo lists
> L = lists:seq(1, 20).
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
> lists:map(fun(X) -> X rem 3 == 0 end, L).
[false,false,true,false,false,true,false,false,true,false,
 false,true,false,false,true,false,false,true,false,false]
> lists:filter(fun(X) -> X rem 3 == 0 end, L).
[3,6,9,12,15,18]
> lists:foldl(fun(Acc, H) -> Acc+H end, 0, L).
210
```
https://github.com/k0dev/erlang-notes/blob/2c42d3d9e40f6f1597025c93c2b0890d66596de0/code/lists_mfr.erl#L1-L15

## List Comprehension
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
> [[A, B] || A<-[a,b], B<-[a,b]]. % permutazioni di [a,b]
[[a,a],[a,b],[b,a],[b,b]]
```
Esempi:
- [quick sort](code/examples/list_comprehension/qsort.erl)
- [numeri primi <= n](code/examples/list_comprehension/primes.erl)

## Concorrenza: introduzione
Erlang mette a disposizione tre funzionalità di base per realizzare la concorrenza:
- `spawn` la funzione built-in (BIF - built-in function) per creare nuovi actors
- `!` l'operatore per inviare un messaggio ad un actor
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
end.
```
L'actor tenta di prendere dalla mailbox il messaggio più vecchio che effettua il match con uno dei pattern. Se nessun messaggio effettua il match, l'actor aspetta indefinitamente in attesa che arrivi un messaggio valido. Se la clausola after è specificata, l'actor aspetta per un tempo massimo di `Expr_t` millisecondi, per poi valutare il contenuto di `Body_t`.
```erlang
receive
  Any -> do_something(Any)
end.
```
In questo esempio l'actor prenderà qualsiasi messaggio, quindi rimane in attesa solo quando la mailbox è vuota.
```erlang
receive
  {Pid, something} -> do_something(Pid)
end.
```
In questo esempio l'actor prende (se esiste) il messaggio più vecchio che effettua il match con `{Pid, something`}. Rimane in attesa fintanto che la mailbox è vuota o non contiene messaggi di questo tipo.

Esempi:
- [conversione temperature](code/examples/message_recv/converter.erl)
- [calcolo aree](code/examples/message_recv/areas.erl)

## Actors registrati
Oltre che riferirci ad un processo mediante il suo pid, sono disponibili delle BIF per registrare un actor sotto un certo nome. Il nome deve essere un atomo e viene automaticamente eliminato se il processo termina.

- `register(atomo, Pid)`
- `registered()`   : restituisce una lista dei nomi registrati
- `unregister(atomo)`
- `whereis(atomo)` : restituisce il pid registrato con il nome `atomo` o `undefined` se il nome non è registrato

Ovviamente una volta assegnato un nome ad un processo è possibile utilizzarlo per inviarli un messaggio (`atomo ! messaggio`).
