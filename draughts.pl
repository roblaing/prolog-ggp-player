/*
http://games.ggp.org/base/games/checkers-mustjump/checkers-mustjump.kif

http://www.wcdf.net/rules.htm

Board Representation (Note, rows opposite to chess)
  a b c d e f g h
1 w g w g w g w g
2
3
4
5
6
7
8 g w g w g w g w

White and Green checkers, with Green on player's left.
Red (top) goes first
*/

:- module(draughts, []).
:- thread_local true/1, does/2.

% red top of board, goes first
role(red).    % owner of wp, wk  
role(black).  % actually white, but owner of bp, bk

init(control(red)).

% cell(Col, Row, Type)
init(cell(a, 1, b)).  init(cell(b, 1, wp)). init(cell(c, 1, b)).  init(cell(d, 1, wp)). init(cell(e, 1, b)).  init(cell(f, 1, wp)). init(cell(g, 1, b)).  init(cell(h, 1, wp)).
init(cell(a, 2, wp)). init(cell(b, 2, b)).  init(cell(c, 2, wp)). init(cell(d, 2, b)).	init(cell(e, 2, wp)). init(cell(f, 2, b)).  init(cell(g, 2, wp)). init(cell(h, 2, b)). 
init(cell(a, 3, b)).  init(cell(b, 3, wp)). init(cell(c, 3, b)).  init(cell(d, 3, wp)).	init(cell(e, 3, b)).  init(cell(f, 3, wp)). init(cell(g, 3, b)).  init(cell(h, 3, wp)).
init(cell(a, 4, b)).  init(cell(b, 4, b)).  init(cell(c, 4, b)).  init(cell(d, 4, b)).	init(cell(e, 4, b)).  init(cell(f, 4, b)).  init(cell(g, 4, b)).  init(cell(h, 4, b)). 
init(cell(a, 5, b)).  init(cell(b, 5, b)).  init(cell(c, 5, b)).  init(cell(d, 5, b)).	init(cell(e, 5, b)).  init(cell(f, 5, b)).  init(cell(g, 5, b)).  init(cell(h, 5, b)). 
init(cell(a, 6, bp)). init(cell(b, 6, b)).  init(cell(c, 6, bp)). init(cell(d, 6, b)).	init(cell(e, 6, bp)). init(cell(f, 6, b)).  init(cell(g, 6, bp)). init(cell(h, 6, b)). 
init(cell(a, 7, b)).  init(cell(b, 7, bp)). init(cell(c, 7, b)).  init(cell(d, 7, bp)).	init(cell(e, 7, b)).  init(cell(f, 7, bp)). init(cell(g, 7, b)).  init(cell(h, 7, bp)).
init(cell(a, 8, bp)). init(cell(b, 8, b)).  init(cell(c, 8, bp)). init(cell(d, 8, b)).	init(cell(e, 8, bp)). init(cell(f, 8, b)).  init(cell(g, 8, bp)). init(cell(h, 8, b)). 

% init(step(1)).
init(piece_count(red, 12)).
init(piece_count(black, 12)).

next(cell(U, V, b)) :-
    does(_Player, move(_P, U, V, _X, _Y)).

next(cell(U, V, b)) :- 
    does(_Player, doublejump(_P, U, V, _X, _Y, _X3, _Y3)).

next(cell(U, V, b)) :- 
    does(_Player, triplejump(_P, U, V, _X, _Y, _X3, _Y3, _X4, _Y4)).

next(cell(X, Y, P)) :- 
    does(_Player, move(P, _U, _V, X, Y)), 
    ( P \== wp ; Y \== 8 ), 
    ( P \== bp ; Y \== 1 ).

next(cell(X, Y, P)) :- 
    does(_Player, doublejump(P, _U, _V, _X3, _Y3, X, Y)), 
    ( P \== wp ; Y \== 8 ), 
    ( P \== bp ; Y \== 1 ).

next(cell(X, Y, P)) :- 
    does(_Player, triplejump(P, _U, _V, _X3, _Y3, _X4, _Y4, X, Y)), 
    ( P \== wp ; Y \== 8 ), 
    ( P \== bp ; Y \== 1 ).

next(cell(X, Y, P)) :- 
    does(Player, move(_Piece, X1, Y1, X2, Y2)), 
    true(cell(X, Y, P)), 
    \+single_jump_capture(Player, X1, Y1, X, Y, X2, Y2), 
    different_cells(X, Y, X1, Y1), 
    different_cells(X, Y, X2, Y2).

next(cell(X, Y, P)) :- 
    does(Player, doublejump(_Piece, X1, Y1, X2, Y2, X3, Y3)), 
    true(cell(X, Y, P)), 
    \+single_jump_capture(Player, X1, Y1, X, Y, X2, Y2), 
    \+single_jump_capture(Player, X2, Y2, X, Y, X3, Y3), 
    different_cells(X, Y, X1, Y1), 
    different_cells(X, Y, X3, Y3).

next(cell(X, Y, P)) :- 
    does(Player, triplejump(_Piece, X1, Y1, X2, Y2, X3, Y3, X4, Y4)), 
    true(cell(X, Y, P)), 
    \+single_jump_capture(Player, X1, Y1, X, Y, X2, Y2), 
    \+single_jump_capture(Player, X2, Y2, X, Y, X3, Y3), 
    \+single_jump_capture(Player, X3, Y3, X, Y, X4, Y4), 
    different_cells(X, Y, X1, Y1), 
    different_cells(X, Y, X4, Y4).

next(cell(X, Y, b)) :- 
    does(Player, move(_Piece, X1, Y1, X2, Y2)), 
    single_jump_capture(Player, X1, Y1, X, Y, X2, Y2).

next(cell(X, Y, b)) :- 
    does(Player, doublejump(_Piece, X1, Y1, X2, Y2, X3, Y3)), 
    ( single_jump_capture(Player, X1, Y1, X, Y, X2, Y2) ; single_jump_capture(Player, X2, Y2, X, Y, X3, Y3) ).

next(cell(X, Y, b)) :- 
    does(Player, triplejump(_Piece, X1, Y1, X2, Y2, X3, Y3, X4, Y4)), 
    ( single_jump_capture(Player, X1, Y1, X, Y, X2, Y2) ; 
      single_jump_capture(Player, X2, Y2, X, Y, X3, Y3) ; 
      single_jump_capture(Player, X3, Y3, X, Y, X4, Y4) ).

next(control(red)) :-
    true(control(black)).

next(control(black)) :- 
    true(control(red)).

% next(step(Y)) :- true(step(X)), successor(X, Y).

next(cell(X, 8, wk)) :- 
    does(red, move(wp, _U, _V, X, 8)).

next(cell(X, 1, bk)) :- 
    does(black, move(bp, _U, _V, X, 1)).

next(cell(X, 8, wk)) :- 
    does(red, doublejump(wp, _U, _V, _X3, _Y3, X, 8)).

next(cell(X, 1, bk)) :- 
    does(black, doublejump(bp, _U, _V, _X3, _Y3, X, 1)).

next(cell(X, 8, wk)) :- 
    does(red, triplejump(_P, _U, _V, _X3, _Y3, _X4, _Y4, X, 8)).

next(cell(X, 1, bk)) :- 
    does(black, triplejump(_P, _U, _V, _X3, _Y3, _X4, _Y4, X, 1)).

next(piece_count(Player, N)) :- 
    ( does(Player, move(_P, _U, _V, _X, _Y)) ; 
      does(Player, doublejump(_P, _U, _V, _X3, _Y3, _X, _Y)) ; 
      does(Player, triplejump(_P, _U, _V, _X3, _Y3, _X4, _Y4, _X, _Y)) ), 
      true(piece_count(Player, N) ).

next(piece_count(red, N)) :- 
    does(black, move(_P, X1, Y1, X2, Y2)), 
    kingmove(black, X1, Y1, X2, Y2), 
    true(piece_count(red, N)).

next(piece_count(red, Lower)) :- 
    does(black, move(_P, X1, Y1, X2, Y2)), 
    single_jump_capture(black, X1, Y1, _X, _Y, X2, Y2), 
    true(piece_count(red, Higher)), 
    Lower is Higher - 1.

next(piece_count(red, Lower)) :- 
    does(black, doublejump(_P, _U, _V, _X3, _Y3, _X, _Y)), 
    true(piece_count(red, Higher)), 
    Lower is Higher - 2.

next(piece_count(red, Lower)) :- 
    does(black, triplejump(_P, _U, _V, _X3, _Y3, _X4, _Y4, _X, _Y)), 
    true(piece_count(red, Higher)), 
    Lower is Higher - 3.

next(piece_count(black, N)) :- 
    does(red, move(_P, X1, Y1, X2, Y2)), 
    kingmove(red, X1, Y1, X2, Y2), 
    true(piece_count(black, N)).

next(piece_count(black, Lower)) :- 
    does(red, move(_P, X1, Y1, X2, Y2)), 
    single_jump_capture(red, X1, Y1, _X, _Y, X2, Y2), 
    true(piece_count(black, Higher)), 
    Lower is Higher - 1.

next(piece_count(black, Lower)) :- 
    does(red, doublejump(_P, _U, _V, _X3, _Y3, _X, _Y)), 
    true(piece_count(black, Higher)), 
    Lower is Higher - 2.

next(piece_count(black, Lower)) :- 
    does(red, triplejump(_P, _U, _V, _X3, _Y3, _X4, _Y4, _X, _Y)), 
    true(piece_count(black, Higher)), 
    Lower is Higher - 3.

legal(Player, move(Piece, U, V, X, Y)) :- 
    true(control(Player)), 
    piece_owner_type(Piece, Player, pawn), 
    true(cell(U, V, Piece)), 
    \+singlejumpavailable(Player), 
    pawnmove(Player, U, V, X, Y), 
    true(cell(X, Y, b)).

legal(Player, move(Piece, U, V, X, Y)) :- 
    true(control(Player)), 
    piece_owner_type(Piece, Player, king), 
    true(cell(U, V, Piece)), 
    \+singlejumpavailable(Player), 
    kingmove(Player, U, V, X, Y), 
    true(cell(X, Y, b)).

legal(Player, move(Piece, U, V, X, Y)) :- 
    true(control(Player)), 
    piece_owner_type(Piece, Player, pawn), 
    true(cell(U, V, Piece)), 
    \+doublejumpavailable(Player), 
    pawnjump(Player, U, V, X, Y), 
    true(cell(X, Y, b)), 
    single_jump_capture(Player, U, V, _C, _D, X, Y).

legal(Player, move(Piece, U, V, X, Y)) :- 
    true(control(Player)), 
    piece_owner_type(Piece, Player, king), 
    true(cell(U, V, Piece)), 
    \+doublejumpavailable(Player), 
    kingjump(Player, U, V, X, Y), 
    true(cell(X, Y, b)), 
    single_jump_capture(Player, U, V, _C, _D, X, Y).

legal(Player, doublejump(Piece, U, V, X1, Y1, X2, Y2)) :- 
    true(control(Player)), 
    piece_owner_type(Piece, Player, pawn), 
    true(cell(U, V, Piece)), 
    \+triplejumpavailable(Player), 
    pawnjump(Player, U, V, X1, Y1), 
    true(cell(X1, Y1, b)), 
    pawnjump(Player, X1, Y1, X2, Y2), 
    true(cell(X2, Y2, b)), 
    different_cells(U, V, X2, Y2), 
    single_jump_capture(Player, U, V, _C, _D, X1, Y1), 
    single_jump_capture(Player, X1, Y1, _C1, _D1, X2, Y2).

legal(Player, doublejump(Piece, U, V, X1, Y1, X2, Y2)) :- 
    true(control(Player)), 
    piece_owner_type(Piece, Player, king), 
    true(cell(U, V, Piece)), 
    \+triplejumpavailable(Player), 
    kingjump(Player, U, V, X1, Y1), 
    true(cell(X1, Y1, b)), 
    kingjump(Player, X1, Y1, X2, Y2), 
    true(cell(X2, Y2, b)), 
    different_cells(U, V, X2, Y2), 
    single_jump_capture(Player, U, V, _C, _D, X1, Y1), 
    single_jump_capture(Player, X1, Y1, _C1, _D1, X2, Y2).

legal(Player, triplejump(Piece, U, V, X1, Y1, X2, Y2, X3, Y3)) :- 
    true(control(Player)), 
    piece_owner_type(Piece, Player, pawn), 
    true(cell(U, V, Piece)), 
    pawnjump(Player, U, V, X1, Y1), 
    true(cell(X1, Y1, b)), 
    pawnjump(Player, X1, Y1, X2, Y2), 
    true(cell(X2, Y2, b)), 
    different_cells(U, V, X2, Y2), 
    pawnjump(Player, X2, Y2, X3, Y3), 
    true(cell(X3, Y3, b)), 
    different_cells(X1, Y1, X3, Y3), 
    single_jump_capture(Player, U, V, _C, _D, X1, Y1), 
    single_jump_capture(Player, X1, Y1, _C1, _D1, X2, Y2), 
    single_jump_capture(Player, X2, Y2, _C2, _D2, X3, Y3).

legal(Player, triplejump(Piece, U, V, X1, Y1, X2, Y2, X3, Y3)) :- 
    true(control(Player)), 
    piece_owner_type(Piece, Player, king), 
    true(cell(U, V, Piece)), 
    kingjump(Player, U, V, X1, Y1), 
    true(cell(X1, Y1, b)), 
    kingjump(Player, X1, Y1, X2, Y2), 
    true(cell(X2, Y2, b)), 
    different_cells(U, V, X2, Y2), 
    kingjump(Player, X2, Y2, X3, Y3), 
    true(cell(X3, Y3, b)), 
    different_cells(X1, Y1, X3, Y3), 
    single_jump_capture(Player, U, V, _C, _D, X1, Y1), 
    single_jump_capture(Player, X1, Y1, _C1, _D1, X2, Y2), 
    single_jump_capture(Player, X2, Y2, _C2, _D2, X3, Y3).

legal(red, noop) :- true(control(black)).
legal(black, noop) :- true(control(red)).

terminal :- 
    true(control(Player)), 
    stuck(Player), !.

terminal :-
    true(piece_count(_Player, 0)).

% terminal :- true(step(60)).

goal(red, 100) :-
    true(piece_count(black, 0)), !.

goal(red, 100) :-
    true(control(black)),
    stuck(black), !.

goal(red, 0) :-
    true(piece_count(red, 0)), !.

goal(red, 0) :-
    true(control(red)),
    stuck(red), !.

goal(black, 100) :-
    true(piece_count(red, 0)), !.

goal(black, 100) :-
    true(control(red)),
    stuck(red), !.
    
goal(black, 0) :-
    true(piece_count(black, 0)), !.

goal(black, 0) :-
    true(control(black)),
    stuck(black), !. 

goal(red, Reward) :-
    \+terminal,
    true(piece_count(red, Red)),
    true(piece_count(black, Black)),
    Reward is 50 + Red - Black.

goal(black, Reward) :-
    \+terminal,
    true(piece_count(red, Red)),
    true(piece_count(black, Black)),
    Reward is 50 + Black - Red.

singlejumpavailable(Player) :- 
    piece_owner_type(Piece, Player, pawn), 
    true(cell(U, V, Piece)), 
    pawnjump(Player, U, V, X, Y), 
    true(cell(X, Y, b)), 
    single_jump_capture(Player, U, V, _C, _D, X, Y).

singlejumpavailable(Player) :- 
    piece_owner_type(Piece, Player, king), 
    true(cell(U, V, Piece)), 
    kingjump(Player, U, V, X, Y), 
    true(cell(X, Y, b)), 
    single_jump_capture(Player, U, V, _C, _D, X, Y).

doublejumpavailable(Player) :- 
    piece_owner_type(Piece, Player, pawn), 
    true(cell(U, V, Piece)), 
    pawnjump(Player, U, V, X1, Y1), 
    true(cell(X1, Y1, b)), 
    pawnjump(Player, X1, Y1, X2, Y2), 
    true(cell(X2, Y2, b)), 
    different_cells(U, V, X2, Y2), 
    single_jump_capture(Player, U, V, _C, _D, X1, Y1), 
    single_jump_capture(Player, X1, Y1, _C1, _D1, X2, Y2).

doublejumpavailable(Player) :- 
    piece_owner_type(Piece, Player, king), 
    true(cell(U, V, Piece)), 
    kingjump(Player, U, V, X1, Y1), 
    true(cell(X1, Y1, b)), 
    kingjump(Player, X1, Y1, X2, Y2), 
    true(cell(X2, Y2, b)), 
    different_cells(U, V, X2, Y2), 
    single_jump_capture(Player, U, V, _C, _D, X1, Y1), 
    single_jump_capture(Player, X1, Y1, _C1, _D1, X2, Y2).

triplejumpavailable(Player) :- 
    piece_owner_type(Piece, Player, pawn), 
    true(cell(U, V, Piece)), 
    pawnjump(Player, U, V, X1, Y1), 
    true(cell(X1, Y1, b)), 
    pawnjump(Player, X1, Y1, X2, Y2), 
    true(cell(X2, Y2, b)), 
    different_cells(U, V, X2, Y2), 
    pawnjump(Player, X2, Y2, X3, Y3), 
    true(cell(X3, Y3, b)), 
    different_cells(X1, Y1, X3, Y3), 
    single_jump_capture(Player, U, V, _C, _D, X1, Y1), 
    single_jump_capture(Player, X1, Y1, _C1, _D1, X2, Y2), 
    single_jump_capture(Player, X2, Y2, _C2, _D2, X3, Y3).

triplejumpavailable(Player) :- 
    piece_owner_type(Piece, Player, king), 
    true(cell(U, V, Piece)), 
    kingjump(Player, U, V, X1, Y1), 
    true(cell(X1, Y1, b)), 
    kingjump(Player, X1, Y1, X2, Y2), 
    true(cell(X2, Y2, b)), 
    different_cells(U, V, X2, Y2), 
    kingjump(Player, X2, Y2, X3, Y3), 
    true(cell(X3, Y3, b)), 
    different_cells(X1, Y1, X3, Y3), 
    single_jump_capture(Player, U, V, _C, _D, X1, Y1), 
    single_jump_capture(Player, X1, Y1, _C1, _D1, X2, Y2), 
    single_jump_capture(Player, X2, Y2, _C2, _D2, X3, Y3).

pawnmove(red, U, V, X, Y) :- 
    next_rank(V, Y), 
    ( next_file(U, X) ; next_file(X, U) ).

pawnmove(black, U, V, X, Y) :- 
    next_rank(Y, V), 
    ( next_file(U, X) ; next_file(X, U) ).

kingmove(Player, U, V, X, Y) :- 
    role(Player), 
    role(Player2), 
    pawnmove(Player2, U, V, X, Y).

pawnjump(red, U, V, X, Y) :- 
    next_rank(V, V1), 
    next_rank(V1, Y), 
    next_file(U, X1), 
    next_file(X1, X).

pawnjump(red, U, V, X, Y) :- 
    next_rank(V, V1), 
    next_rank(V1, Y), 
    next_file(X, X1), 
    next_file(X1, U).

pawnjump(black, U, V, X, Y) :- 
    next_rank(Y, V1), 
    next_rank(V1, V), 
    next_file(U, X1), 
    next_file(X1, X).

pawnjump(black, U, V, X, Y) :- 
    next_rank(Y, V1), 
    next_rank(V1, V), 
    next_file(X, X1), 
    next_file(X1, U).

kingjump(Player, U, V, X, Y) :- 
    role(Player), 
    role(Player2), 
    pawnjump(Player2, U, V, X, Y).

single_jump_capture(Player, U, V, C, D, X, Y) :- 
    kingjump(Player, U, V, X, Y), 
    kingmove(Player, U, V, C, D), 
    kingmove(Player, C, D, X, Y), 
    true(cell(C, D, Piece)), 
    opponent(Player, Opponent), 
    piece_owner_type(Piece, Opponent, _Type).

has_legal_move(Player) :- 
    piece_owner_type(Piece, Player, _Type), 
    ( legal(Player, move(Piece, _U, _V, _X, _Y)) ; 
      legal(Player, doublejump(Piece, _U, _V, _A, _B, _X, _Y)) ; 
      legal(Player, triplejump(Piece, _U, _V, _A, _B, _C, _D, _X, _Y)) ).

stuck(Player) :- 
    role(Player), 
    \+has_legal_move(Player).

adjacent(X1, X2) :-
    next_file(X1, X2).

adjacent(X1, X2) :-
    next_file(X2, X1).

adjacent(Y1, Y2) :-
    next_rank(Y1, Y2).

adjacent(Y1, Y2) :-
    next_rank(Y2, Y1).

different_cells(X1, Y1, X2, Y2) :-
    ( X1 \== X2 ; Y1 \== Y2 ).

opponent(red, black).
opponent(black, red).

piece_owner_type(wk, red, king).
piece_owner_type(wp, red, pawn).
piece_owner_type(bk, black, king).
piece_owner_type(bp, black, pawn).
next_rank(1, 2).
next_rank(2, 3).
next_rank(3, 4).
next_rank(4, 5).
next_rank(5, 6).
next_rank(6, 7).
next_rank(7, 8).
next_file(a, b).
next_file(b, c).
next_file(c, d).
next_file(d, e).
next_file(e, f).
next_file(f, g).
next_file(g, h).
odd_rank(1).
odd_rank(3).
odd_rank(5).
odd_rank(7).
even_rank(2).
even_rank(4).
even_rank(6).
even_rank(8).
rank(Y) :- odd_rank(Y).
rank(Y) :- even_rank(Y).
odd_file(a).
odd_file(c).
odd_file(e).
odd_file(g).
even_file(b).
even_file(d).
even_file(f).
even_file(h).
file(X) :- odd_file(X).
file(X) :- even_file(X).

odd_square(X, Y) :- 
    odd_file(X), 
    even_rank(Y).

odd_square(X, Y) :- 
    even_file(X), 
    odd_rank(Y).

even_square(X, Y) :- 
    odd_file(X), 
    odd_rank(Y).

even_square(X, Y) :- 
    even_file(X), 
    even_rank(Y).

base(cell(X, Y, b)) :- file(X), rank(Y).
base(cell(X, Y, King)) :- odd_square(X, Y), piece_owner_type(King, _Player, king).
base(cell(X, Y, wp)) :- odd_square(X, Y), dif(Y, c8).
base(cell(X, Y, bp)) :- odd_square(X, Y), dif(Y, c1).
base(control(Player)) :- role(Player).

% base(step(N)) :- succ(Nm1, N).

base(piece_count(Player, N)) :- 
    role(Player), 
    succ(N, _Np1), 
    13 > N.

input(Player, move(Piece, U, V, X, Y)) :- piece_owner_type(Piece, Player, pawn), odd_square(U, V), pawnmove(Player, U, V, X, Y).
input(Player, move(Piece, U, V, X, Y)) :- piece_owner_type(Piece, Player, king), odd_square(U, V), kingmove(Player, U, V, X, Y).
input(Player, move(Piece, U, V, X, Y)) :- piece_owner_type(Piece, Player, pawn), odd_square(U, V), pawnjump(Player, U, V, X, Y).
input(Player, move(Piece, U, V, X, Y)) :- piece_owner_type(Piece, Player, king), odd_square(U, V), kingjump(Player, U, V, X, Y).
input(Player, doublejump(Piece, U, V, X1, Y1, X2, Y2)) :- 
    piece_owner_type(Piece, Player, pawn), 
    odd_square(U, V), 
    pawnjump(Player, U, V, X1, Y1), 
    pawnjump(Player, X1, Y1, X2, Y2), 
    different_cells(U, V, X2, Y2).
input(Player, doublejump(Piece, U, V, X1, Y1, X2, Y2)) :- 
    piece_owner_type(Piece, Player, king), 
    odd_square(U, V), 
    kingjump(Player, U, V, X1, Y1), 
    kingjump(Player, X1, Y1, X2, Y2), 
    different_cells(U, V, X2, Y2).
input(Player, triplejump(Piece, U, V, X1, Y1, X2, Y2, X3, Y3)) :- 
    piece_owner_type(Piece, Player, pawn), 
    odd_square(U, V), 
    pawnjump(Player, U, V, X1, Y1), 
    pawnjump(Player, X1, Y1, X2, Y2), 
    different_cells(U, V, X2, Y2), 
    pawnjump(Player, X2, Y2, X3, Y3), 
    different_cells(X1, Y1, X3, Y3).
input(Player, triplejump(Piece, U, V, X1, Y1, X2, Y2, X3, Y3)) :- 
    piece_owner_type(Piece, Player, king), 
    odd_square(U, V), 
    kingjump(Player, U, V, X1, Y1), 
    kingjump(Player, X1, Y1, X2, Y2), 
    different_cells(U, V, X2, Y2), 
    kingjump(Player, X2, Y2, X3, Y3), 
    different_cells(X1, Y1, X3, Y3).
input(Player, noop) :- role(Player).

% Heuristic function added to fix wonky provided goal value for non-terminals

utility(Player, Value) :- goal(Player, Value).
