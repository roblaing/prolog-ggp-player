:- module(tictactoe, []).

:- thread_local true/1, does/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tictactoe
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Components
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

role(white).
role(black).

base(cell(M, N, x)) :- index(M), index(N).
base(cell(M, N, o)) :- index(M), index(N).
base(cell(M, N, b)) :- index(M), index(N).
base(control(white)).
base(control(black)).

input(R, mark(M, N)) :- role(R), index(M), index(N).
input(R, noop) :- role(R).

index(1).
index(2).
index(3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% init
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(cell(1, 1, b)).
init(cell(1, 2, b)).
init(cell(1, 3, b)).
init(cell(2, 1, b)).
init(cell(2, 2, b)).
init(cell(2, 3, b)).
init(cell(3, 1, b)).
init(cell(3, 2, b)).
init(cell(3, 3, b)).
init(control(white)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% legal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

legal(W, mark(X, Y)) :-
  true(cell(X, Y, b)),
  true(control(W)).

legal(white, noop) :-
  true(control(black)).

legal(black, noop) :-
  true(control(white)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% next
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

next(cell(M, N, x)) :-
  does(white, mark(M, N)),
  true(cell(M, N, b)).

next(cell(M, N, o)) :-
  does(black, mark(M, N)),
  true(cell(M, N, b)).

next(cell(M, N, W)) :-
  true(cell(M, N, W)),
  W \== b.

next(cell(M, N, b)) :-
  true(cell(M, N, b)),
  \+does(_, mark(M, N)).

next(control(white)) :-
  true(control(black)).
    
next(control(black)) :-
  true(control(white)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% goal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

line(X) :-
  true(cell(M, 1, X)),
  true(cell(M, 2, X)),
  true(cell(M, 3, X)).

line(X) :-
  true(cell(1, N, X)),
  true(cell(2, N, X)),
  true(cell(3, N, X)).

line(X) :-
  true(cell(1, 1, X)),
  true(cell(2, 2, X)),
  true(cell(3, 3, X)).

line(X) :-
  true(cell(1, 3, X)),
  true(cell(2, 2, X)),
  true(cell(3, 1, X)).

open :- true(cell(_, _, b)).

goal(white, 100) :-
  line(x),
  \+line(o).
    
goal(white, 50) :-
  \+line(x),
  \+line(o).
    
goal(white, 0) :-
  \+line(x),
  line(o).

goal(black, 100) :-
  \+line(x),
  line(o).

goal(black, 50) :-
  \+line(x),
  \+line(o).

goal(black, 0) :-
  line(x),
  \+line(o).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% terminal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

terminal :- line(x).
terminal :- line(o).
terminal :- \+open.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

utility(white, 51) :-
  goal(white, 50),
  does(white, mark(3, 3)),
  true(cell(1, 1, x)), !.

utility(white, 51) :-
  goal(white, 50),
  does(white, mark(1, 1)),
  true(cell(3, 3, x)), !.

utility(white, 51) :-
  goal(white, 50),
  does(white, mark(1, 3)),
  true(cell(3, 1, x)), !.

utility(white, 51) :-
  goal(white, 50),
  does(white, mark(3, 1)),
  true(cell(1, 3, x)), !.

utility(white, 49) :-
  does(white, noop),
  utility(black, 51), !.

utility(white, Value) :- goal(white, Value).

utility(black, 51) :-
  goal(black, 50),
  does(black, mark(3, 3)),
  true(cell(1, 1, o)), !.

utility(black, 51) :-
  goal(black, 50),
  does(black, mark(1, 1)),
  true(cell(3, 3, o)), !.

utility(black, 51) :-
  goal(black, 50),
  does(black, mark(1, 3)),
  true(cell(3, 1, o)), !.

utility(black, 51) :-
  goal(black, 50),
  does(black, mark(3, 1)),
  true(cell(1, 3, o)), !.

utility(black, 49) :-
  does(black, noop),
  utility(white, 51), !.

utility(black, Value) :- goal(black, Value).
