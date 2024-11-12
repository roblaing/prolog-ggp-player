:- module(linesofaction, []).

/** <module> Lines of Action rules

This game, designed by Claude Soucie, is among those selected by Sid Sackson in his book A Gamut of Games.

I thought it would make a nice Prolog project because I could use a [kif version](https://games.ggp.org/base/games/linesOfAction/linesOfAction.kif)
done for Stanford University's General Game Playing course as a starting point.

A direct translation from kif to Prolog didn't work (it hung the computer), so this ended up being translated from the "paper rules" rather than the kif code.

@author Robert Laing
@license GPL
*/

:- thread_local true/1, does/2.

/** 
 * role(?Player:atom) is nondet
 *
 * This is a 2 player game. The GDL convention appears to list players in their order of play, so this is a black goes first game.
 */


role(black).
role(white).

/** 
 * init(?Base:term) is nondet
 *
 * This provides the starting squares for black and white checkers pieces, and who's turn it is.
 *
 * ```prolog
 * retractall(true(_)), forall(init(Base), assertz(true(Base))).
 * ```
 * would set this to the current state for `legal(Player, Move).`
 */

init(cell(b, 1, black)).
init(cell(c, 1, black)).
init(cell(d, 1, black)).
init(cell(e, 1, black)).
init(cell(f, 1, black)).
init(cell(g, 1, black)).
init(cell(b, 8, black)).
init(cell(c, 8, black)).
init(cell(d, 8, black)).
init(cell(e, 8, black)).
init(cell(f, 8, black)).
init(cell(g, 8, black)).
init(cell(a, 2, white)).
init(cell(a, 3, white)).
init(cell(a, 4, white)).
init(cell(a, 5, white)).
init(cell(a, 6, white)).
init(cell(a, 7, white)).
init(cell(h, 2, white)).
init(cell(h, 3, white)).
init(cell(h, 4, white)).
init(cell(h, 5, white)).
init(cell(h, 6, white)).
init(cell(h, 7, white)).

init(control(black)).

/**
 * legal(?Player:atom, ?Move:term) is nondet
 *
 * This returns the move options for a given player in a given state. Positions of the pieces and who's turn it is comes from reading true(Base).
 * A piece can move from its current square in a straight line horizontal, vertical or diagonal (ie eight compass points) the number of squares
 * equal to the number of pieces (either side) in that line. A player may jump the piece over friendly pieces, but is blocked by opponent pieces.
 * The final square may contain an opponent piece (removing it from the board), but not a friendly piece.
 */


legal(Player, noop):-
  role(Player),
  \+true(control(Player)).

% n  increases rank (row) since 1 is at bottom
legal(Player, move(X, Y1, X, Y2)):-
  role(Player),
  true(control(Player)),
  true(cell(X, Y1, Player)),
  aggregate_all(count, [Rank, Role], true(cell(X, Rank, Role)), Spaces),
  move_n(Player, X, Y1, Spaces, X, Y2),
  \+true(cell(X, Y2, Player)).
  
% ne
legal(Player, move(X1, Y1, X2, Y2)):-
  role(Player),
  true(control(Player)),
  true(cell(X1, Y1, Player)),
  diagonal_ne(X1, Y1, Diagonal),
  aggregate_all(count, [File, Rank, Role], (
    true(cell(File, Rank, Role)), diagonal_ne(File, Rank, Diagonal)
  ), Spaces),
  move_ne(Player, X1, Y1, Spaces, X2, Y2),
  \+true(cell(X2, Y2, Player)).

% e increases file (column)
legal(Player, move(X1, Y, X2, Y)):-
  role(Player),
  true(control(Player)),
  true(cell(X1, Y, Player)),
  aggregate_all(count, [File, Role], true(cell(File, Y, Role)), Spaces),
  move_e(Player, X1, Y, Spaces, X2, Y),
  \+true(cell(X2, Y, Player)).

% se
legal(Player, move(X1, Y1, X2, Y2)):-
  role(Player),
  true(control(Player)),
  true(cell(X1, Y1, Player)),
  diagonal_se(X1, Y1, Diagonal),
  aggregate_all(count, [File, Rank, Role], (
    true(cell(File, Rank, Role)), diagonal_se(File, Rank, Diagonal)
  ), Spaces),
  move_se(Player, X1, Y1, Spaces, X2, Y2),
  \+true(cell(X2, Y2, Player)).

% s
legal(Player, move(X, Y1, X, Y2)):-
  role(Player),
  true(control(Player)),
  true(cell(X, Y1, Player)),
  aggregate_all(count, [Rank, Role], true(cell(X, Rank, Role)), Spaces),
  move_s(Player, X, Y1, Spaces, X, Y2),
  \+true(cell(X, Y2, Player)).

% sw
legal(Player, move(X1, Y1, X2, Y2)):-
  role(Player),
  true(control(Player)),
  true(cell(X1, Y1, Player)),
  diagonal_ne(X1, Y1, Diagonal),
  aggregate_all(count, [File, Rank, Role], (
    true(cell(File, Rank, Role)), diagonal_ne(File, Rank, Diagonal)
  ), Spaces),
  move_sw(Player, X1, Y1, Spaces, X2, Y2),
  \+true(cell(X2, Y2, Player)).

% w
legal(Player, move(X1, Y, X2, Y)):-
  role(Player),
  true(control(Player)),
  true(cell(X1, Y, Player)),
  aggregate_all(count, [File, Role], true(cell(File, Y, Role)), Spaces),
  move_w(Player, X1, Y, Spaces, X2, Y),
  \+true(cell(X2, Y, Player)).

% nw
legal(Player, move(X1, Y1, X2, Y2)):-
  role(Player),
  true(control(Player)),
  true(cell(X1, Y1, Player)),
  diagonal_se(X1, Y1, Diagonal),
  aggregate_all(count, [File, Rank, Role], (
    true(cell(File, Rank, Role)), diagonal_se(File, Rank, Diagonal)
  ), Spaces),
  move_nw(Player, X1, Y1, Spaces, X2, Y2),
  \+true(cell(X2, Y2, Player)).

/* noop for control player if no other moves possible */
legal(Player, noop):-
  findall(move(X1, Y1, X2, Y2), legal(Player, move(X1, Y1, X2, Y2)), L),
  length(L, 0).

/**
 * next(?Base:term) is nondet
 *
 * This assumes `does(Player, Move)` which is one of the `legal(Player, Move)` moves has been asserted along with the required `true(Base)`
 * to describe the current state.
 */

% change congrol
next(control(white)) :-
  true(control(black)).
next(control(black)) :-
  true(control(white)).

% moved to cell
next(cell(X2, Y2, Player)) :-
  does(Player, move(_X1, _Y1, X2, Y2)).

% copy unaffected cells from current state, skipping moved from cell and possibly captured cell

next(cell(X, Y, Role)) :-
  true(cell(X, Y, Role)),
  \+affected(X, Y).

% next helpers

affected(X1,Y1) :- 
  does(_Player, move(X1, Y1, _X2, _Y2)).
affected(X2,Y2) :-
  does(_Player, move(_X1, _Y1, X2, Y2)).


/**
 * terminal:Boolean is det
 *
 * The game is over when one of the players has managed to combine all their pieces into a connected cluster.
 */
 
terminal :- all_connected(black).
terminal :- all_connected(white).

/**
 * goal(Player:player, Value:integer) is det
 *
 * Winner gets 100, loser 0
 *
 * To assist the AI player pick a move, rather than the default 50/50 GDL goal value (which means either a draw or game still underway), I've
 * developed a `utility(Player, Value)` guestimate which adds points for centrality and size of biggest cluster so far.
 */

goal(black, 100) :- all_connected(black), \+all_connected(white).
goal(white, 0)   :- all_connected(black), \+all_connected(white).
goal(white, 100) :- all_connected(white), \+all_connected(black).
goal(black, 0)   :- all_connected(white), \+all_connected(black).
goal(black, 50)  :- \+all_connected(black), \+all_connected(white).
goal(black, 50)  :- all_connected(black), all_connected(white).
goal(white, 50)  :- \+all_connected(black), \+all_connected(white).
goal(white, 50)  :- all_connected(black), all_connected(white).

% legal helpers

enemy_block(Player, File, Rank, Spaces) :-
  Spaces > 0,
  true(cell(File, Rank, Role)),
  Role \= Player.


move_n(_Player, File, Rank, 0, File, Rank) :- !.

move_n(Player, File, Rank1, Spaces1, File, Rank) :-
  Spaces2 is Spaces1 - 1,
  nextRank(Rank1, Rank2),
  \+enemy_block(Player, File, Rank2, Spaces2),
  move_n(Player, File, Rank2, Spaces2, File, Rank).

move_ne(_Player, File, Rank, 0, File, Rank) :- !.

move_ne(Player, File1, Rank1, Spaces1, File, Rank) :-
  Spaces2 is Spaces1 - 1,
  nextRank(Rank1, Rank2),
  nextFile(File1, File2),
  \+enemy_block(Player, File2, Rank2, Spaces2),
  move_ne(Player, File2, Rank2, Spaces2, File, Rank).

move_e(_Player, File, Rank, 0, File, Rank) :- !.

move_e(Player, File1, Rank, Spaces1, File, Rank) :-
  Spaces2 is Spaces1 - 1,
  nextFile(File1, File2),
  \+enemy_block(Player, File2, Rank, Spaces2),
  move_e(Player, File2, Rank, Spaces2, File, Rank).

move_se(_Player, File, Rank, 0, File, Rank) :- !.

move_se(Player, File1, Rank1, Spaces1, File, Rank) :-
  Spaces2 is Spaces1 - 1,
  nextRank(Rank2, Rank1),
  nextFile(File1, File2),
  \+enemy_block(Player, File2, Rank2, Spaces2),
  move_se(Player, File2, Rank2, Spaces2, File, Rank).

move_s(_Player, File, Rank, 0, File, Rank) :- !.

move_s(Player, File, Rank1, Spaces1, File, Rank) :-
  Spaces2 is Spaces1 - 1,
  nextRank(Rank2, Rank1),
  \+enemy_block(Player, File, Rank2, Spaces2),
  move_s(Player, File, Rank2, Spaces2, File, Rank).

move_sw(_Player, File, Rank, 0, File, Rank) :- !.

move_sw(Player, File1, Rank1, Spaces1, File, Rank) :-
  Spaces2 is Spaces1 - 1,
  nextRank(Rank2, Rank1),
  nextFile(File2, File1),
  \+enemy_block(Player, File2, Rank2, Spaces2),
  move_sw(Player, File2, Rank2, Spaces2, File, Rank).

move_w(_Player, File, Rank, 0, File, Rank) :- !.

move_w(Player, File1, Rank, Spaces1, File, Rank) :-
  Spaces2 is Spaces1 - 1,
  nextFile(File2, File1),
  \+enemy_block(Player, File2, Rank, Spaces2),
  move_w(Player, File2, Rank, Spaces2, File, Rank).

move_nw(_Player, File, Rank, 0, File, Rank) :- !.

move_nw(Player, File1, Rank1, Spaces1, File, Rank) :-
  Spaces2 is Spaces1 - 1,
  nextRank(Rank1, Rank2),
  nextFile(File2, File1),
  \+enemy_block(Player, File2, Rank2, Spaces2),
  move_nw(Player, File2, Rank2, Spaces2, File, Rank).

% Chess Board

% a is left column, h is right column
nextFile(a, b).
nextFile(b, c).
nextFile(c, d).
nextFile(d, e).
nextFile(e, f).
nextFile(f, g).
nextFile(g, h).

% 1 is bottom row, 8 is top row
nextRank(1, 2).
nextRank(2, 3).
nextRank(3, 4).
nextRank(4, 5).
nextRank(5, 6).
nextRank(6, 7).
nextRank(7, 8).

diagonal_ne(a, 1, a1).
diagonal_ne(b, 2, a1).
diagonal_ne(c, 3, a1).
diagonal_ne(d, 4, a1).
diagonal_ne(e, 5, a1).
diagonal_ne(f, 6, a1).
diagonal_ne(g, 7, a1).
diagonal_ne(h, 8, a1).
diagonal_ne(a, 2, a2).
diagonal_ne(b, 3, a2).
diagonal_ne(c, 4, a2).
diagonal_ne(d, 5, a2).
diagonal_ne(e, 6, a2).
diagonal_ne(f, 7, a2).
diagonal_ne(g, 8, a2).
diagonal_ne(a, 3, a3).
diagonal_ne(b, 4, a3).
diagonal_ne(c, 5, a3).
diagonal_ne(d, 6, a3).
diagonal_ne(e, 7, a3).
diagonal_ne(f, 8, a3).
diagonal_ne(a, 4, a4).
diagonal_ne(b, 5, a4).
diagonal_ne(c, 6, a4).
diagonal_ne(d, 7, a4).
diagonal_ne(e, 8, a4).
diagonal_ne(a, 5, a5).
diagonal_ne(b, 6, a5).
diagonal_ne(c, 7, a5).
diagonal_ne(d, 8, a5).
diagonal_ne(a, 6, a6).
diagonal_ne(b, 7, a6).
diagonal_ne(c, 8, a6).
diagonal_ne(a, 7, a7).
diagonal_ne(b, 8, a7).
diagonal_ne(a, 8, a8).
diagonal_ne(b, 1, b1).
diagonal_ne(c, 2, b1).
diagonal_ne(d, 3, b1).
diagonal_ne(e, 4, b1).
diagonal_ne(f, 5, b1).
diagonal_ne(g, 6, b1).
diagonal_ne(h, 7, b1).
diagonal_ne(c, 1, c1).
diagonal_ne(d, 2, c1).
diagonal_ne(e, 3, c1).
diagonal_ne(f, 4, c1).
diagonal_ne(g, 5, c1).
diagonal_ne(h, 6, c1).
diagonal_ne(d, 1, d1).
diagonal_ne(e, 2, d1).
diagonal_ne(f, 3, d1).
diagonal_ne(g, 4, d1).
diagonal_ne(h, 5, d1).
diagonal_ne(e, 1, e1).
diagonal_ne(f, 2, e1).
diagonal_ne(g, 3, e1).
diagonal_ne(h, 4, e1).
diagonal_ne(f, 1, f1).
diagonal_ne(g, 2, f1).
diagonal_ne(h, 3, f1).
diagonal_ne(g, 1, g1).
diagonal_ne(h, 2, g1).
diagonal_ne(h, 1, h1).

diagonal_se(a, 8, a8).
diagonal_se(b, 7, a8).
diagonal_se(c, 6, a8).
diagonal_se(d, 5, a8).
diagonal_se(e, 4, a8).
diagonal_se(f, 3, a8).
diagonal_se(g, 2, a8).
diagonal_se(h, 1, a8).
diagonal_se(a, 7, a7).
diagonal_se(b, 6, a7).
diagonal_se(c, 5, a7).
diagonal_se(d, 4, a7).
diagonal_se(e, 3, a7).
diagonal_se(f, 2, a7).
diagonal_se(g, 1, a7).
diagonal_se(a, 6, a6).
diagonal_se(b, 5, a6).
diagonal_se(c, 4, a6).
diagonal_se(d, 3, a6).
diagonal_se(e, 2, a6).
diagonal_se(f, 1, a6).
diagonal_se(a, 5, a5).
diagonal_se(b, 4, a5).
diagonal_se(c, 3, a5).
diagonal_se(d, 2, a5).
diagonal_se(e, 1, a5).
diagonal_se(a, 4, a4).
diagonal_se(b, 3, a4).
diagonal_se(c, 2, a4).
diagonal_se(d, 1, a4).
diagonal_se(a, 3, a3).
diagonal_se(b, 2, a3).
diagonal_se(c, 1, a3).
diagonal_se(a, 2, a2).
diagonal_se(b, 1, a2).
diagonal_se(a, 1, a1).
diagonal_se(b, 8, b8).
diagonal_se(c, 7, b8).
diagonal_se(d, 6, b8).
diagonal_se(e, 5, b8).
diagonal_se(f, 4, b8).
diagonal_se(g, 3, b8).
diagonal_se(h, 2, b8).
diagonal_se(c, 8, c8).
diagonal_se(d, 7, c8).
diagonal_se(e, 6, c8).
diagonal_se(f, 5, c8).
diagonal_se(g, 4, c8).
diagonal_se(h, 3, c8).
diagonal_se(d, 8, d8).
diagonal_se(e, 7, d8).
diagonal_se(f, 6, d8).
diagonal_se(g, 5, d8).
diagonal_se(h, 4, d8).
diagonal_se(e, 8, e8).
diagonal_se(f, 7, e8).
diagonal_se(g, 6, e8).
diagonal_se(h, 5, e8).
diagonal_se(f, 8, f8).
diagonal_se(g, 7, f8).
diagonal_se(h, 6, f8).
diagonal_se(g, 8, g8).
diagonal_se(h, 7, g8).
diagonal_se(h, 8, h8).

% victory conditions

% all_connected

all_connected(Player) :-
  all_pieces(Player, [StartCell|AllPieces]),
  length([StartCell|AllPieces], L1),
  cell_cluster(StartCell, Cluster),
  length(Cluster, L2),
  L1 == L2.
  

/**
 *  cell_cluster(+StartCell, -Cluster) is nondet.
 */

cell_cluster(StartCell, Cluster) :-
  cell_cluster_([StartCell], [], Cluster).

cell_cluster_([], Visited, Cluster) :-
  sort(Visited, Cluster).

cell_cluster_([Cursor|Frontier], Visited, Cluster) :-
  member(Cursor, Visited),
  cell_cluster_(Frontier, Visited, Cluster).

cell_cluster_([Cursor|Frontier], Visited, Cluster) :-
  \+member(Cursor, Visited),
  get_neighbours(Cursor, Neighbours),
  append(Neighbours, Frontier, Unsorted),
  sort(Unsorted, NewFrontier),
  cell_cluster_(NewFrontier, [Cursor|Visited], Cluster).

all_pieces(Player, Pieces) :-
  setof(cell(X, Y, Player), true(cell(X, Y, Player)), Pieces).

get_neighbours(cell(X1, Y1, Player), Neighbours) :-
  neighbour_n(cell(X1, Y1, Player), [], N),
  sort(N, Neighbours).

neighbour_n(cell(X1, Y1, Player), L, Neighbours) :-
  \+nextRank(Y1, _Y2),
  neighbour_ne(cell(X1, Y1, Player), L, Neighbours).

neighbour_n(cell(X1, Y1, Player), L, Neighbours) :-
  nextRank(Y1, Y2),
  true(cell(X1, Y2, Player)),
  neighbour_ne(cell(X1, Y1, Player), [cell(X1, Y2, Player)|L], Neighbours).

neighbour_n(cell(X1, Y1, Player), L, Neighbours) :-
  nextRank(Y1, Y2),
  \+true(cell(X1, Y2, Player)),
  neighbour_ne(cell(X1, Y1, Player), L, Neighbours).

neighbour_ne(cell(X1, Y1, Player), L, Neighbours) :-
  \+nextFile(X1, _X2),
  neighbour_e(cell(X1, Y1, Player), L, Neighbours).

neighbour_ne(cell(X1, Y1, Player), L, Neighbours) :-
  \+nextRank(Y1, _Y2),
  neighbour_e(cell(X1, Y1, Player), L, Neighbours).

neighbour_ne(cell(X1, Y1, Player), L, Neighbours) :-
  nextFile(X1, X2),
  nextRank(Y1, Y2),
  true(cell(X2, Y2, Player)),
  neighbour_e(cell(X1, Y1, Player), [cell(X2, Y2, Player)|L], Neighbours).

neighbour_ne(cell(X1, Y1, Player), L, Neighbours) :-
  nextFile(X1, X2),
  nextRank(Y1, Y2),
  \+true(cell(X2, Y2, Player)),
  neighbour_e(cell(X1, Y1, Player), L, Neighbours).

neighbour_e(cell(X1, Y1, Player), L, Neighbours) :-
  \+nextFile(X1, _X2),
  neighbour_se(cell(X1, Y1, Player), L, Neighbours).

neighbour_e(cell(X1, Y1, Player), L, Neighbours) :-
  nextFile(X1, X2),
  true(cell(X2, Y1, Player)),
  neighbour_se(cell(X1, Y1, Player), [cell(X2, Y1, Player)|L], Neighbours).

neighbour_e(cell(X1, Y1, Player), L, Neighbours) :-
  nextFile(X1, X2),
  \+true(cell(X2, Y1, Player)),
  neighbour_se(cell(X1, Y1, Player), L, Neighbours).

neighbour_se(cell(X1, Y1, Player), L, Neighbours) :-
  \+nextFile(X1, _X2),
  neighbour_s(cell(X1, Y1, Player), L, Neighbours).

neighbour_se(cell(X1, Y1, Player), L, Neighbours) :-
  \+nextRank(_Y2, Y1),
  neighbour_s(cell(X1, Y1, Player), L, Neighbours).

neighbour_se(cell(X1, Y1, Player), L, Neighbours) :-
  nextFile(X1, X2),
  nextRank(Y2, Y1),
  true(cell(X2, Y2, Player)),
  neighbour_s(cell(X1, Y1, Player), [cell(X2, Y2, Player)|L], Neighbours).

neighbour_se(cell(X1, Y1, Player), L, Neighbours) :-
  nextFile(X1, X2),
  nextRank(Y2, Y1),
  \+true(cell(X2, Y2, Player)),
  neighbour_s(cell(X1, Y1, Player), L, Neighbours).

neighbour_s(cell(X1, Y1, Player), L, Neighbours) :-
  \+nextRank(_Y2, Y1),
  neighbour_sw(cell(X1, Y1, Player), L, Neighbours).

neighbour_s(cell(X1, Y1, Player), L, Neighbours) :-
  nextRank(Y2, Y1),
  true(cell(X1, Y2, Player)),
  neighbour_sw(cell(X1, Y1, Player), [cell(X1, Y2, Player)|L], Neighbours).

neighbour_s(cell(X1, Y1, Player), L, Neighbours) :-
  nextRank(Y2, Y1),
  \+true(cell(X1, Y2, Player)),
  neighbour_sw(cell(X1, Y1, Player), L, Neighbours).

neighbour_sw(cell(X1, Y1, Player), L, Neighbours) :-
  \+nextFile(_X2, X1),
  neighbour_w(cell(X1, Y1, Player), L, Neighbours).

neighbour_sw(cell(X1, Y1, Player), L, Neighbours) :-
  \+nextRank(_Y2, Y1),
  neighbour_w(cell(X1, Y1, Player), L, Neighbours).

neighbour_sw(cell(X1, Y1, Player), L, Neighbours) :-
  nextFile(X2, X1),
  nextRank(Y2, Y1),
  true(cell(X2, Y2, Player)),
  neighbour_w(cell(X1, Y1, Player), [cell(X2, Y2, Player)|L], Neighbours).

neighbour_sw(cell(X1, Y1, Player), L, Neighbours) :-
  nextFile(X2, X1),
  nextRank(Y2, Y1),
  \+true(cell(X2, Y2, Player)),
  neighbour_w(cell(X1, Y1, Player), L, Neighbours).

neighbour_w(cell(X1, Y1, Player), L, Neighbours) :-
  \+nextFile(_X2, X1),
  neighbour_nw(cell(X1, Y1, Player), L, Neighbours).

neighbour_w(cell(X1, Y1, Player), L, Neighbours) :-
  nextFile(X2, X1),
  true(cell(X2, Y1, Player)),
  neighbour_nw(cell(X1, Y1, Player), [cell(X2, Y1, Player)|L], Neighbours).

neighbour_w(cell(X1, Y1, Player), L, Neighbours) :-
  nextFile(X2, X1),
  \+true(cell(X2, Y1, Player)),
  neighbour_nw(cell(X1, Y1, Player), L, Neighbours).

neighbour_nw(cell(X1, _Y1, _Player), L, L) :-
  \+nextFile(_X2, X1).

neighbour_nw(cell(_X1, Y1, _Player), L, L) :-
  \+nextRank(Y1, _Y2).

neighbour_nw(cell(X1, Y1, Player), L, [cell(X2, Y2, Player)|L]) :-
  nextFile(X2, X1),
  nextRank(Y1, Y2),
  true(cell(X2, Y2, Player)).

neighbour_nw(cell(X1, Y1, Player), L, L) :-
  nextFile(X2, X1),
  nextRank(Y1, Y2),
  \+true(cell(X2, Y2, Player)).


% maybe make more complex using size of largest cluster to help AI player?

utility(black, 100) :- all_connected(black), \+all_connected(white).
utility(white, 0)   :- all_connected(black), \+all_connected(white).
utility(white, 100) :- all_connected(white), \+all_connected(black).
utility(black, 0)   :- all_connected(white), \+all_connected(black).
utility(black, 50)  :- all_connected(black), all_connected(white).
utility(white, 50)  :- all_connected(black), all_connected(white).

utility(black, Value) :- 
  \+all_connected(black), \+all_connected(white),
  max_cluster_size(black, MaxClusterSize),
  centrality_sum(black, Sum),
  Value is 50 + MaxClusterSize + Sum.

utility(white, Value) :- 
  \+all_connected(black), \+all_connected(white),
  max_cluster_size(white, MaxClusterSize),
  centrality_sum(white, Sum),
  Value is 50 + MaxClusterSize + Sum.


max_cluster_size(Player, MaxClusterSize) :-
  all_pieces(Player, AllPieces),
  max_cluster_size_(Player, AllPieces, 0, MaxClusterSize).

max_cluster_size_(_Player, [], MaxClusterSize, MaxClusterSize) :- !.

max_cluster_size_(Player, [StartCell|AllPieces], Size1, MaxClusterSize) :-
  cluster_size(StartCell, Cluster, Size2),
  max_list([Size1, Size2], Size3),
  subtract([StartCell|AllPieces], Cluster, RemainingPieces),
  max_cluster_size_(Player, RemainingPieces, Size3, MaxClusterSize).

cluster_size(StartCell, Cluster, Size) :-
  cell_cluster(StartCell, Cluster),
  length(Cluster, Size).

centrality(cell(c, 3, black), 1).
centrality(cell(c, 4, black), 1).
centrality(cell(c, 5, black), 1).
centrality(cell(c, 6, black), 1).
centrality(cell(d, 3, black), 1).
centrality(cell(d, 4, black), 2).
centrality(cell(d, 5, black), 2).
centrality(cell(d, 6, black), 1).
centrality(cell(e, 3, black), 1).
centrality(cell(e, 4, black), 2).
centrality(cell(e, 5, black), 2).
centrality(cell(e, 6, black), 1).
centrality(cell(f, 3, black), 1).
centrality(cell(f, 4, black), 1).
centrality(cell(f, 5, black), 1).
centrality(cell(f, 6, black), 1).
centrality(cell(c, 3, white), 1).
centrality(cell(c, 4, white), 1).
centrality(cell(c, 5, white), 1).
centrality(cell(c, 6, white), 1).
centrality(cell(d, 3, white), 1).
centrality(cell(d, 4, white), 2).
centrality(cell(d, 5, white), 2).
centrality(cell(d, 6, white), 1).
centrality(cell(e, 3, white), 1).
centrality(cell(e, 4, white), 2).
centrality(cell(e, 5, white), 2).
centrality(cell(e, 6, white), 1).
centrality(cell(f, 3, white), 1).
centrality(cell(f, 4, white), 1).
centrality(cell(f, 5, white), 1).
centrality(cell(f, 6, white), 1).

centrality_sum(Player, Sum) :-
  aggregate_all(sum(Val), (true(cell(X, Y, Player)), centrality(cell(X, Y, Player), Val)), Sum).
