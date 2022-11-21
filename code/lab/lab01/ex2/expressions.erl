-module(expressions).
-export([parse/1, evaluate/1]).

% assumo che ogni token sia al massimo un carattere (anche i numeri)


%% Evaluation %%

evaluate({num, N})       -> N;
evaluate({plus, A, B})   -> evaluate(A) + evaluate(B);
evaluate({minus, A, B})  -> evaluate(A) - evaluate(B);
evaluate({divide, A, B}) -> evaluate(A) / evaluate(B);
evaluate({times, A, B})  -> evaluate(A) * evaluate(B);
evaluate({invert, A})    -> -evaluate(A).

%% Parsing %%

% uso https://en.wikipedia.org/wiki/Shunting_yard_algorithm per trasformare
% un espressione comune parentesizzata in una in notazione polacca

next_token([]) -> end_of_expr;
next_token([H|T]) -> {H, T}.

create_expression([], [E | []])      -> E;
create_expression([$+|T], [A,B|Acc]) -> create_expression(T, [{plus, B, A} | Acc]);
create_expression([$-|T], [A,B|Acc]) -> create_expression(T, [{minus, B, A} | Acc]);
create_expression([$/|T], [A,B|Acc]) -> create_expression(T, [{divide, B, A} | Acc]);
create_expression([$*|T], [A,B|Acc]) -> create_expression(T, [{times, B, A} | Acc]);
create_expression([$~|T], [A|Acc]) -> create_expression(T, [{invert, A} | Acc]);
create_expression([H|T], Acc) -> create_expression(T, [{num, list_to_integer([H])} | Acc]).
  

parse(Str) -> create_expression(parse(next_token(Str), [], []), []).

parse(end_of_expr, Output, Operators) -> lists:reverse(lists:append(Operators, Output));
parse({Token, Rest}, Output, Operators) when 
    Token >= $0, Token =< $9 -> parse(next_token(Rest), [Token|Output], Operators);
parse({$(, Rest}, Output, Operators) -> parse(next_token(Rest), Output, [$(|Operators]);
parse({$), Rest}, Output, Operators) -> parse_close_par(Rest, Output, Operators);
parse({Op1, Rest}, Output, [Op2|T]) when
    Op2 =/= $(, not (((Op1 =:= $*) or (Op1 =:= $/)) and ((Op2 =:= $+) or (Op2 =:= $-))) -> % per la precedenza degli operatori
      parse({Op1, Rest}, [Op2|Output], T);
parse({Op, Rest}, Output, Operators) -> parse(next_token(Rest), Output, [Op|Operators]).

parse_close_par(Rest, Output, [$(|T]) -> parse(next_token(Rest), Output, T);
parse_close_par(Rest, Output, [H|T])  -> parse_close_par(Rest, [H|Output], T);
parse_close_par(_, _, []) -> exit(unbalanced_par).
