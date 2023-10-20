:- module(chinese_checkers2, []).

/**  <module> chess rules
 *   @author Robert Laing
 *   @see <http://games.ggp.org/base/games/chineseCheckers2/chineseCheckers2.kif>
*/

:- thread_local true/1, does/2.


role(red).
role(teal).

affected(X) :- does(_Player, move(X, _Z)).

base(cell(C, R)) :- index(C), role(R).
base(control(R)) :- role(R).
% base(step(N)) :- between(1, 40, N).
% base(step(1)).
input(R, move(C1, C2)) :- role(R), car(R, C1, C2).
input(R, move(C1, C2)) :- role(R), cdr(R, C1, C2).
input(R, move(C1, C3)) :- role(R), car(R, C1, C2), car(R, C2, C3).
input(R, move(C1, C3)) :- role(R), cdr(R, C1, C2), cdr(R, C2, C3).
input(R, move(C1, C5)) :- role(R), car(R, C1, C2), car(R, C2, C3), car(R, C3, C4), car(R, C4, C5).
input(R, move(C1, C5)) :- role(R), car(R, C1, C2), car(R, C2, C3), cdr(R, C3, C4), cdr(R, C4, C5).
input(R, move(C1, C5)) :- role(R), cdr(R, C1, C2), cdr(R, C2, C3), car(R, C3, C4), car(R, C4, C5).
input(R, move(C1, C5)) :- role(R), cdr(R, C1, C2), cdr(R, C2, C3), cdr(R, C3, C4), cdr(R, C4, C5).
input(R, noop) :- role(R).
index(a1).
index(b1).
index(b2).
index(c1).
index(c2).
index(c3).
index(c4).
index(c5).
index(c6).
index(c7).
index(d1).
index(d2).
index(d3).
index(d4).
index(d5).
index(d6).
index(e1).
index(e2).
index(e3).
index(e4).
index(e5).
index(f1).
index(f2).
index(f3).
index(f4).
index(f5).
index(f6).
index(g1).
index(g2).
index(g3).
index(g4).
index(g5).
index(g6).
index(g7).
index(h1).
index(h2).
index(i1).
init(cell(a1, red)).
init(cell(b1, red)).
init(cell(b2, red)).
init(cell(h1, teal)).
init(cell(h2, teal)).
init(cell(i1, teal)).
init(control(red)).
% init(step(1)).

next(cell(E, Player)) :- does(Player, move(_B, E)).
next(cell(X, Z)) :- true(cell(X, Z)), \+(affected(X)).
next(control(teal)) :- true(control(red)).
next(control(red)) :- true(control(teal)).
% next(step(Y)) :- true(step(X)), successor(X, Y).

successor(N, M) :-
  succ(N, M),
  N > 0,
  M < 41.

legal(Player, move(B, E)) :- true(control(Player)), movable(Player, B, E).

legal(Player, noop) :- role(Player).

occupied(C) :-
  true(cell(C, _P)).

empty(C) :- index(C), \+(occupied(C)).
movable(Player, B, E) :- true(cell(B, Player)), car(Player, B, C), occupied(C), car(Player, C, X), empty(X), car(Player, X, D), occupied(D), car(Player, D, E), empty(E).
movable(Player, B, E) :- true(cell(B, Player)), car(Player, B, C), occupied(C), car(Player, C, X), empty(X), cdr(Player, X, D), occupied(D), cdr(Player, D, E), empty(E).
movable(Player, B, E) :- true(cell(B, Player)), cdr(Player, B, C), occupied(C), cdr(Player, C, X), empty(X), car(Player, X, D), occupied(D), car(Player, D, E), empty(E).
movable(Player, B, E) :- true(cell(B, Player)), cdr(Player, B, C), occupied(C), cdr(Player, C, X), empty(X), cdr(Player, X, D), occupied(D), cdr(Player, D, E), empty(E).
movable(Player, B, E) :- true(cell(B, Player)), car(Player, B, C), occupied(C), car(Player, C, E), empty(E).
movable(Player, B, E) :- true(cell(B, Player)), cdr(Player, B, C), occupied(C), cdr(Player, C, E), empty(E).
movable(Player, B, E) :- true(cell(B, Player)), car(Player, B, E), empty(E).
movable(Player, B, E) :- true(cell(B, Player)), cdr(Player, B, E), empty(E).
car(red, B, E) :- carred(B, E).
car(teal, B, E) :- carred(E, B).
cdr(red, B, E) :- cdrred(B, E).
cdr(teal, B, E) :- cdrred(E, B).
carred(a1, b1).
carred(b1, c3).
carred(b2, c4).
carred(c3, d2).
carred(c4, d3).
carred(c5, d4).
carred(d2, e1).
carred(d3, e2).
carred(d4, e3).
carred(d5, e4).
carred(e2, f2).
carred(e3, f3).
carred(e4, f4).
carred(e5, f5).
carred(f3, g3).
carred(f4, g4).
carred(f5, g5).
carred(g4, h1).
carred(g5, h2).
carred(h2, i1).
cdrred(a1, b2).
cdrred(b1, c4).
cdrred(b2, c5).
cdrred(c3, d3).
cdrred(c4, d4).
cdrred(c5, d5).
cdrred(d2, e2).
cdrred(d3, e3).
cdrred(d4, e4).
cdrred(d5, e5).
cdrred(e1, f2).
cdrred(e2, f3).
cdrred(e3, f4).
cdrred(e4, f5).
cdrred(f2, g3).
cdrred(f3, g4).
cdrred(f4, g5).
cdrred(g3, h1).
cdrred(g4, h2).
cdrred(h1, i1).

tocount(red, h1, h2, i1).
tocount(teal, a1, b1, b2).

stones(Player, 100) :-
  tocount(Player, X, Y, Z),
  true(cell(X, Player)),
  true(cell(Y, Player)),
  true(cell(Z, Player)).

stones(Player, 50) :-
  tocount(Player, X, Y, Z),
  true(cell(X, Player)),
  true(cell(Y, Player)),
  \+(true(cell(Z, Player))).

stones(Player, 50) :- tocount(Player, X, Y, Z), true(cell(X, Player)), \+(true(cell(Y, Player))), true(cell(Z, Player)).

stones(Player, 50) :- tocount(Player, X, Y, Z), \+(true(cell(X, Player))), true(cell(Y, Player)), true(cell(Z, Player)).

stones(Player, 25) :- tocount(Player, X, Y, Z), true(cell(X, Player)), \+(true(cell(Y, Player))), \+(true(cell(Z, Player))).

stones(Player, 25) :- tocount(Player, X, Y, Z), \+(true(cell(X, Player))), true(cell(Y, Player)), \+(true(cell(Z, Player))).

stones(Player, 25) :- tocount(Player, X, Y, Z), \+(true(cell(X, Player))), \+(true(cell(Y, Player))), true(cell(Z, Player)).

stones(Player, 0) :- terminal, tocount(Player, X, Y, Z), \+(true(cell(X, Player))), \+(true(cell(Y, Player))), \+(true(cell(Z, Player))).

% modified so that no points doesn't mean game lost
stones(Player, 1) :- \+ terminal, tocount(Player, X, Y, Z), \+(true(cell(X, Player))), \+(true(cell(Y, Player))), \+(true(cell(Z, Player))).


goal(Player, Payoff) :-
  stones(Player, Payoff).

% terminal :- true(step(40)).
terminal :-
  stones(_Player, 100).

% need to remember to add this to each set of rules

move_val(does(Player, move(B,E)), 1) :-
  car(Player, B, E).

move_val(does(Player, move(B,E)), 1) :-
  cdr(Player, B, E).

move_val(does(Player, move(B,E)), 2) :-
  car(Player, B, C), car(Player, C, E).

move_val(does(Player, move(B,E)), 2) :-
  cdr(Player, B, C), cdr(Player, C, E).

move_val(does(Player, move(B,E)), 3) :-
  car(Player, B, C), 
  car(Player, C, X), 
  car(Player, X, D), 
  car(Player, D, E).

move_val(does(Player, move(B,E)), 3) :-
  car(Player, B, C), 
  car(Player, C, X), 
  cdr(Player, X, D), 
  cdr(Player, D, E).

move_val(does(Player, move(B,E)), 3) :-
  cdr(Player, B, C), 
  cdr(Player, C, X), 
  car(Player, X, D), 
  car(Player, D, E).

move_val(does(Player, move(B,E)), 3) :-
  cdr(Player, B, C), 
  cdr(Player, C, X), 
  cdr(Player, X, D), 
  cdr(Player, D, E).

utility(Player, 100) :-
  stones(Player, 100).

utility(Player, Payoff) :-
  does(Player, noop),
  stones(Player, Payoff).

utility(Player, Payoff) :-
  does(Player, move(B,E)),
  move_val(does(Player, move(B,E)), Val),
  stones(Player, P),
  P < 100,
  Payoff is P + Val.

