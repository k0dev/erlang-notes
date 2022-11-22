-module(expressions).
-export([parse/1, evaluate/1]).

%% assumo che ogni token sia al massimo un carattere (anche i numeri) %%

%% Evaluation %%

evaluate({num, N})       -> N;
evaluate({plus, A, B})   -> evaluate(A) + evaluate(B);
evaluate({minus, A, B})  -> evaluate(A) - evaluate(B);
evaluate({divide, A, B}) -> evaluate(A) / evaluate(B);
evaluate({times, A, B})  -> evaluate(A) * evaluate(B);
evaluate({invert, A})    -> -evaluate(A).

%% Parsing %%

% Shunting yard algorithm (https://en.wikipedia.org/wiki/Shunting_yard_algorithm):
% per trasformare un espressione aritmetica in una in notazione polacca inversa 

next_token([]) -> end_of_expr;
next_token([H|T]) -> {H, T}.

create_expression([], [E | []])      -> E;
create_expression([$+|T], [A,B|Acc]) -> create_expression(T, [{plus, B, A} | Acc]);
create_expression([$-|T], [A,B|Acc]) -> create_expression(T, [{minus, B, A} | Acc]);
create_expression([$/|T], [A,B|Acc]) -> create_expression(T, [{divide, B, A} | Acc]);
create_expression([$*|T], [A,B|Acc]) -> create_expression(T, [{times, B, A} | Acc]);
create_expression([$~|T], [A|Acc])   -> create_expression(T, [{invert, A} | Acc]);
create_expression([H|T], Acc)        -> create_expression(T, [{num, list_to_integer([H])} | Acc]).
 
parse(Str) -> create_expression(parse(next_token(Str), [], []), []).

% caso base, abbiamo finito di parsare la stringa ->
%     while there are tokens on the operator stack:
%       /* If the operator token on the top of the stack is a parenthesis, then there are mismatched parentheses. */
%       {assert the operator on top of the stack is not a (left) parenthesis}
%       pop the operator from the operator stack onto the output queue
parse(end_of_expr, Output, Operators) -> 
  case lists:member($(, Operators) of
    false -> lists:append(lists:reverse(Output), Operators);
    true  -> exit(unbalanced_par)
  end;

% il prossimo token è una cifra -> lo aggiungo all'output
parse({Token, Rest}, Output, Operators) when 
    Token >= $0, Token =< $9 -> parse(next_token(Rest), [Token|Output], Operators);

% il prossimo token è una parentesi aperta -> la aggiungo agli operatori
parse({$(, Rest}, Output, Operators) -> parse(next_token(Rest), Output, [$(|Operators]);

% il prossimo token è una parentesi chiusa -> 
%         while the operator at the top of the operator stack is not a left parenthesis:
%            {assert the operator stack is not empty}
%            /* If the stack runs out without finding a left parenthesis, then there are mismatched parentheses. */
%            pop the operator from the operator stack into the output queue
%         pop the left parenthesis from the operator stack and discard it
parse({$), Rest}, Output, Operators) -> parse_close_par(Rest, Output, Operators);

% il prossimo token è un operatore o1 ->
%         while (
%            there is an operator o2 other than the left parenthesis at the top
%            of the operator stack, and (o2 has greater precedence than o1
%            or they have the same precedence and o1 is left-associative)
%          ):
%            pop o2 from the operator stack into the output queue
parse({Op1, Rest}, Output, [Op2|T]) when
    Op2 =/= $(, not(((Op1 =:= $*) or (Op1 =:= $/)) and ((Op2 =:= $+) or (Op2 =:= $-))) -> 
        parse({Op1, Rest}, [Op2|Output], T);

% push o1 onto the operator stack
parse({Op, Rest}, Output, Operators) -> parse(next_token(Rest), Output, [Op|Operators]).

% pop the left parenthesis from the operator stack and discard it
parse_close_par(Rest, Output, [$(|T]) -> parse(next_token(Rest), Output, T);
% pop the operator from the operator stack into the output queue
parse_close_par(Rest, Output, [H|T])  -> parse_close_par(Rest, [H|Output], T);
% operator stack is empty -> mismatched parentheses
parse_close_par(_, _, []) -> exit(unbalanced_par).
