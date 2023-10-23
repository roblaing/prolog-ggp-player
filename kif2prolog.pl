#!/usr/bin/env swipl

:- use_module(library(http/http_open)).
:- use_module(library(dcg/basics)).

:- initialization(main, main).


main(Argv) :-
  last(Argv, Url),
  setup_call_cleanup(
    http_open(Url, Stream, []),
    phrase_from_stream(lines(_Lines), Stream),
    close(Stream)
  ).

lines([Line|Lines]) --> line(Line), !, lines(Lines).
lines([])           --> [].

line(Comment)    --> comment(Cs), { string_codes(Comment, Cs), format('~w~n', [Comment]) }, !.
line(Whitespace) --> whitespace(Cs), { string_codes(Whitespace, Cs), format('~w', [Whitespace]) }, !.
line(Sexp)      --> "(", s_expression(Sexp), ")", { format('~w.', [Sexp]) }.

s_expression(Sexp) --> blanks, term(Operator), blanks, terms(Args), { sexp(Operator, Args, Sexp) }.

sexp('<=', [Head|List], Sexp) :- !,
  comma_list(Body,List),
  Sexp =.. [':-',Head,Body].

sexp('not', Args, Sexp) :- !,
  Sexp =.. ['\\+'|Args].

sexp('or', Args, SemicolonList) :- !,
  semicolon_list(SemicolonList, Args).

sexp('distinct', Args, Sexp) :- !,
  Sexp =.. ['\\='|Args].

sexp(Operator, Args, Sexp) :-
  Sexp =.. [Operator|Args].

terms([Term|Terms]) --> blanks, term(Term), blanks, !, terms(Terms).
terms([])           --> [].

term(Sexp) --> "(", s_expression(Sexp), ")".
term(Variable) --> variable(Cs), { string_codes(Variable, Cs) }.
term(Symbol)   --> symbol(Cs), { atom_codes(Symbol, Cs) }.   

variable([U|Cs])  --> [63,L], { to_upper(L, U) }, symbol_rest(Cs).
symbol([C|Cs])    --> [C], { code_type(C, graph), string_codes("?();", Codes), \+ memberchk(C, Codes) }, symbol_rest(Cs).
symbol_rest([C|Cs]) --> [C], { code_type(C, graph), string_codes("();", Codes), \+ memberchk(C, Codes) }, symbol_rest(Cs).
symbol_rest([])     --> [].

comment([37|Cs])  --> ";", comment_rest(Cs).
comment_rest([37|Cs])  --> ";", comment_rest(Cs).
comment_rest([C|Cs])  --> [C], { \+ code_type(C, end_of_line) }, comment_rest(Cs).
comment_rest([])  --> [C], { code_type(C, end_of_line) }.

whitespace([C|Cs])    --> [C], { code_type(C, space) }, whitespace_rest(Cs).
whitespace_rest([])   --> [], !.
whitespace_rest([C|Cs])    --> [C], { code_type(C, space) }, whitespace_rest(Cs).
