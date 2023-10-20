:- module(chess, []).

/**  <module> chess rules
 *   @author Robert Laing
 *   @see <http://games.ggp.org/base/games/chess/chess.kif>
*/

:- thread_local true/1, does/2.

% removed step(N) from state to avoid duplicate states in database

% http://games.ggp.org/base/games/chess/chess.kif
%*******************************************************************************
%* chess.kif                                                                   *
%* First Draft: Tom Fawcett - 2/23/06                                          *
%* Edits: Nat Love, David Stracuzzi                                            *
%* Last revision 5/10/06 by Nat Love                                           *
%*******************************************************************************

role(white).
role(black).

%*******************************************************************************
%* Initial state.                                                              *
%* Letters are columns: row 1 is WHITE side, row 8 is BLACK                    *
%* Numbers are rows:    column a is left, h is right (from white side)         *
%*******************************************************************************

init(cell(a,8,br)). init(cell(b,8,bn)). init(cell(c,8,bb)). init(cell(d,8,bq)). init(cell(e,8,bk)). init(cell(f,8,bb)). init(cell(g,8,bn)). init(cell(h,8,br)).
init(cell(a,7,bp)). init(cell(b,7,bp)). init(cell(c,7,bp)). init(cell(d,7,bp)). init(cell(e,7,bp)). init(cell(f,7,bp)). init(cell(g,7,bp)). init(cell(h,7,bp)).
init(cell(a,6,b)). init(cell(b,6,b)). init(cell(c,6,b)). init(cell(d,6,b)). init(cell(e,6,b)). init(cell(f,6,b)). init(cell(g,6,b)). init(cell(h,6,b)).
init(cell(a,5,b)). init(cell(b,5,b)). init(cell(c,5,b)). init(cell(d,5,b)). init(cell(e,5,b)). init(cell(f,5,b)). init(cell(g,5,b)). init(cell(h,5,b)).
init(cell(a,4,b)). init(cell(b,4,b)). init(cell(c,4,b)). init(cell(d,4,b)). init(cell(e,4,b)). init(cell(f,4,b)). init(cell(g,4,b)). init(cell(h,4,b)).
init(cell(a,3,b)). init(cell(b,3,b)). init(cell(c,3,b)). init(cell(d,3,b)). init(cell(e,3,b)). init(cell(f,3,b)). init(cell(g,3,b)). init(cell(h,3,b)).
init(cell(a,2,wp)). init(cell(b,2,wp)). init(cell(c,2,wp)). init(cell(d,2,wp)). init(cell(e,2,wp)). init(cell(f,2,wp)). init(cell(g,2,wp)). init(cell(h,2,wp)).
init(cell(a,1,wr)). init(cell(b,1,wn)). init(cell(c,1,wb)). init(cell(d,1,wq)). init(cell(e,1,wk)). init(cell(f,1,wb)). init(cell(g,1,wn)). init(cell(h,1,wr)).
init(control(white)).

%*******************************************************************************
%* NEXT STATE AXIOMS: REGULAR MOVES                                            *
%*******************************************************************************

% MOVE SOURCE
% Piece P moves out of column U, row V leaving square blank
next(cell(U, V, b)) :-
  does(_Player, move(_P, U, V, _X, _Y)).

% MOVE DESTINATION: NON-QUEENING MOVE
% Piece P moves to column X, row Y
next(cell(X, Y, P)) :-
  does(_Player, move(P, _U, _V, X, Y)), 
  (dif(P, wp) ; dif(Y, 8)), 
  (dif(P, bp) ; dif(Y, 1)).

% UNDISTURBED CELL: NON-CASTLE MOVE / NON-ENPASSANT
% Piece (or blank) ?p at ?x ?y remains unchanged if:
% 1) This move is not a castle or an en passant capture
% 2) ?x ?y is a different cell from the move source cell
% 3) ?x ?y is a different cell from the move destination cell
next(cell(X, Y, P)) :-
  does(Player, move(Piece, X1, Y1, X2, Y2)), 
  true(cell(X, Y, P)),
  different_cells(X, Y, X1, Y1), 
  different_cells(X, Y, X2, Y2),
  \+is_castle_move(Piece, X1, Y1, X2, Y2), 
  \+pawn_capture_en_passant(Player, X1, Y1, X2, Y2). 

% CONTROL TRANSFER
next(control(white)) :- true(control(black)).
next(control(black)) :- true(control(white)).

%*******************************************************************************
%* NEXT STATE AXIOMS: SPECIAL MOVES                                            *
%*******************************************************************************

% MOVE DESTINATION: QUEENING MOVE
next(cell(X, 8, wq)) :- 
  does(_Player, move(wp, _U, _V, X, 8)).
next(cell(X, 1, bq)) :- 
  does(_Player, move(bp, _U, _V, X, 1)).

% UNDISTURBED CELL: CASTLE MOVE
% Piece or blank ?p at ?x ?y remains unchanged if:
% 1) This is a castle move
% 2) ?x ?y is not one of the 4 or 5 castle cells
next(cell(X, Y, P)) :-
  does(_Player, move(Q, X1, Y1, X2, Y2)), 
  true(cell(X, Y, P)), 
  is_castle_move(Q, X1, Y1, X2, Y2), 
  \+is_castle_cell(X2, Y2, X, Y).

% UNDISTURBED CELL: EN PASSANT
% Piece or blank ?p at ?x ?y remains unchanged if:
% 1) This is an en passant capture
% 2) ?x ?y is not one of the en passant cells
next(cell(X, Y, P)) :-
  does(Player, move(_Piece, X1, Y1, X2, Y2)), 
  true(cell(X, Y, P)), 
  pawn_capture_en_passant(Player, X1, Y1, X2, Y2), 
  different_cells(X, Y, X1, Y1), 
  different_cells(X, Y, X2, Y2), 
  different_cells(X, Y, X2, Y1).

% PAWN CAPTURED EN PASSANT
next(cell(X2, Y1, b)) :-
  does(Player, move(_Piece, X1, Y1, X2, Y2)), 
  pawn_capture_en_passant(Player, X1, Y1, X2, Y2).

% CASTLING: KING SIDE WHITE
next(cell(e, 1, b)) :- 
  does(white, move(wk, e, 1, g, 1)).
next(cell(f, 1, wr)) :- 
  does(white, move(wk, e, 1, g, 1)).
next(cell(g, 1, wk)) :- 
  does(white, move(wk, e, 1, g, 1)).
next(cell(h, 1, b)) :- 
  does(white, move(wk, e, 1, g, 1)).

% CASTLING: QUEEN SIDE WHITE
next(cell(a, 1, b)) :- 
  does(white, move(wk, e, 1, c, 1)).
next(cell(b, 1, b)) :- 
  does(white, move(wk, e, 1, c, 1)).
next(cell(c, 1, wk)) :- 
  does(white, move(wk, e, 1, c, 1)).
next(cell(d, 1, wr)) :- 
  does(white, move(wk, e, 1, c, 1)).
next(cell(e, 1, b)) :- 
  does(white, move(wk, e, 1, c, 1)).

% CASTLING: KING SIDE BLACK
next(cell(e, 8, b)) :- 
  does(black, move(bk, e, 8, g, 8)).
next(cell(f, 8, br)) :- 
  does(black, move(bk, e, 8, g, 8)).
next(cell(g, 8, bk)) :- 
  does(black, move(bk, e, 8, g, 8)).
next(cell(h, 8, b)) :- 
  does(black, move(bk, e, 8, g, 8)).

% CASTLING: QUEEN SIDE BLACK
next(cell(a, 8, b)) :- 
  does(black, move(bk, e, 8, c, 8)).
next(cell(b, 8, b)) :- 
  does(black, move(bk, e, 8, c, 8)).
next(cell(c, 8, bk)) :- 
  does(black, move(bk, e, 8, c, 8)).
next(cell(d, 8, br)) :- 
  does(black, move(bk, e, 8, c, 8)).
next(cell(e, 8, b)) :- 
  does(black, move(bk, e, 8, c, 8)).

%*******************************************************************************
%* NEXT STATE AXIOMS: SPECIAL STATE PREDICATES                                 *
%*******************************************************************************

% PIECE_HAS_MOVED
% True iff rook or king has moved from original position.
% Used to test legality of castle move.
next(piece_has_moved(wk, e, 1)) :- 
  does(white, move(wk, e, 1, _X, _Y)).
next(piece_has_moved(wr, a, 1)) :- 
  does(white, move(wr, a, 1, _X, _Y)).
next(piece_has_moved(wr, h, 1)) :- 
  does(white, move(wr, h, 1, _X, _Y)).
next(piece_has_moved(bk, e, 8)) :- 
  does(black, move(bk, e, 8, _X, _Y)).
next(piece_has_moved(br, a, 8)) :- 
  does(black, move(br, a, 8, _X, _Y)).
next(piece_has_moved(br, h, 8)) :- 
  does(black, move(br, h, 8, _X, _Y)).

next(piece_has_moved(P, X, Y)) :- 
  true(piece_has_moved(P, X, Y)).

% PAWN MOVED TWO
% True iff this pawn just advanced 2 spots.
% Used to test legality of en passant pawn capture.
% No frame axiom for this since it only lasts one state.
next(pawn_moved_two(wp, X)) :- 
  does(white, move(wp, X, 2, X, 4)).
next(pawn_moved_two(bp, X)) :- 
  does(black, move(bp, X, 7, X, 5)).

% KING IN CHECK
% True iff Player's king is in check from opponent's Piece at X Y
next(check(Player, pawn, X, Y)) :- 
  does(Opponent, move(Piece, _U, _V, X, Y)), 
  opponent(Player, Opponent), 
  piece_owner_type(Piece, Opponent, pawn), 
  piece_owner_type(King, Player, king), 
  true(cell(Kx, Ky, King)), 
  pawn_capture(X, Y, Kx, Ky, Opponent).

next(check(Player, knight, X, Y)) :- 
  does(Opponent, move(Piece, _U, _V, X, Y)), 
  opponent(Player, Opponent), 
  piece_owner_type(Piece, Opponent, knight), 
  piece_owner_type(King, Player, king), 
  true(cell(Kx, Ky, King)), 
  knight_move(Piece, X, Y, Kx, Ky, Opponent).

next(check(Player, rook, X, Y)) :- 
  does(Opponent, move(Piece, _U, _V, X, Y)), 
  opponent(Player, Opponent), 
  piece_owner_type(Piece, Opponent, rook), 
  piece_owner_type(King, Player, king), 
  true(cell(Kx, Ky, King)), 
  rook_move(X, Y, Kx, Ky).

next(check(Player, bishop, X, Y)) :-
  does(Opponent, move(Piece, _U, _V, X, Y)), 
  opponent(Player, Opponent), 
  piece_owner_type(Piece, Opponent, bishop), 
  piece_owner_type(King, Player, king), 
  true(cell(Kx, Ky, King)), 
  bishop_move(X, Y, Kx, Ky).

next(check(Player, queen, X, Y)) :- 
  does(Opponent, move(Piece, _U, _V, X, Y)), 
  opponent(Player, Opponent), 
  piece_owner_type(Piece, Opponent, queen), 
  piece_owner_type(King, Player, king), 
  true(cell(Kx, Ky, King)), 
  queen_move(X, Y, Kx, Ky).
  
% KING IN CHECK (DISCOVERED)
% You can be placed in check by an opponent moving a piece which exposes a 
% threat from another piece. (Threating piece must be bishop, rook, or queen).

% Original generated check when a piece was in the way of a rook 
next(check(Player, rook, Rx, Ry)) :-
  does(Opponent, move(_Piece, Px1, Py1, Px2, Py2)),
  opponent(Player, Opponent), 
  piece_owner_type(Rook, Opponent, rook), 
  piece_owner_type(King, Player, king), 
  true(cell(Rx, Ry, Rook)), 
  true(cell(Kx, Ky, King)),
  rook_threatens(Opponent, Rx, Ry, Kx, Ky, Px1, Py1, Px2, Py2).

next(check(Player, bishop, Bx, By)) :- 
  does(Opponent, move(_Piece, Px1, Py1, Px2, Py2)), 
  opponent(Player, Opponent), 
  piece_owner_type(Bishop, Opponent, bishop), 
  piece_owner_type(King, Player, king), 
  true(cell(Bx, By, Bishop)), 
  true(cell(Kx, Ky, King)), 
  bishop_threatens(Opponent, Bx, By, Kx, Ky, Px1, Py1, Px2, Py2).

next(check(Player, queen, Qx, Qy)) :-
  does(Opponent, move(_Piece, Px1, Py1, Px2, Py2)), 
  opponent(Player, Opponent), 
  piece_owner_type(Queen, Opponent, queen), 
  piece_owner_type(King, Player, king), 
  true(cell(Qx, Qy, Queen)), 
  true(cell(Kx, Ky, King)),
  queen_threatens(Opponent, Qx, Qy, Kx, Ky, Px1, Py1, Px2, Py2).

% Need to add case of pawn promotion
next(check(black, queen, X, 8)) :-
  does(white, move(wp, U, 7, X, 8)),
  true(cell(Kx, Ky, bk)),
  queen_move_ig(X, 8, Kx, Ky, U, 7, X, 8).

next(check(white, queen, X, 1)) :-
  does(black, move(bp, U, 2, X, 1)),
  true(cell(Kx, Ky, wk)),
  queen_move_ig(X, 1, Kx, Ky, U, 2, X, 1).

%*******************************************************************************
%* LEGAL MOVES and their auxilliary axioms                                     *
%*******************************************************************************

/**
 *  legal(?Player:atom, ?Move:compound) is nondet
 *
 *  Returns all legal moves for what is currently true.
 *
 * @param Player obtained from true(control(Player))
 * @param Move move(Piece, U, V, X, Y)
 * @param Piece obtained from true(cell(U, V, Piece)), along with U and V
 * @param X obtained from legal2(Player, move(Piece, U, V, X, Y)) along with Y
 *
 */

% Legal Move when you are:
% 1) NOT in check
% 2) NOT moving your King
% 3) Combined Not CAPTURING and CAPTURING by using occupied_by_opponent_or_blank(X, Y, Player),
legal(Player, move(Piece, U, V, X, Y)) :- 
  true(control(Player)), 
  \+true(check(Player, _TType, _Tx, _Ty)),
  piece_owner_type(Piece, Player, Ptype), 
  Ptype \== king,
  true(cell(U, V, Piece)), % Added, since it seems to be missing in kif file
  legal2(Player, move(Piece, U, V, X, Y)),
  occupied_by_opponent_or_blank(X, Y, Player),
  piece_owner_type(King, Player, king), 
  true(cell(Kx, Ky, King)), 
  \+threatened_with_capture(Player, X, Y, Kx, Ky, U, V).

% Legal Moves when you are:
% 1) In check
% 2) Capturing threatening piece.
legal(Player, move(Piece, U, V, X, Y)) :- 
  true(control(Player)), 
  true(check(Player, _AType, X, Y)),
  piece_owner_type(Piece, Player, Ptype), 
  Ptype \== king,
  true(cell(U, V, Piece)),
  legal2(Player, move(Piece, U, V, X, Y)), 
  % occupied_by_opponent(X, Y, Player), % Already established by true(check(Player, _AType, X, Y)),
  piece_owner_type(King, Player, king), 
  true(cell(Kx, Ky, King)), 
  \+threatened_with_capture(Player, X, Y, Kx, Ky, U, V).

% Legal Moves when you are:
% 1) In check
% 2) Blocking threatening piece
% Only queens, rooks, and bishops can be blocked. We treat block like capture

% Block rook threat (rook or queen)
legal(Player, move(Piece, U, V, X, Y)) :- 
  true(control(Player)), 
  ( true(check(Player, rook,  Tx, Ty)) 
  ; true(check(Player, queen, Tx, Ty)) ),
  piece_owner_type(Piece, Player, Ptype), 
  Ptype \== king,
  true(cell(U, V, Piece)), % Added, since it seems to be missing in kif file
  legal2(Player, move(Piece, U, V, X, Y)),
  piece_owner_type(King, Player, king), 
  true(cell(Kx, Ky, King)),
  blocks_rook_threat(X, Y, Tx, Ty, Kx, Ky),
  occupied_by_opponent_or_blank(X, Y, Player),   
  \+threatened_with_capture(Player, X, Y, Kx, Ky, U, V).

% Block bishop threat (bishop or queen)
legal(Player, move(Piece, U, V, X, Y)) :- 
  true(control(Player)), 
  ( true(check(Player, bishop, Tx, Ty)) 
  ; true(check(Player, queen,  Tx, Ty)) ),
  piece_owner_type(Piece, Player, Ptype), 
  Ptype \== king,
  true(cell(U, V, Piece)), % Added, since it seems to be missing in kif file
  legal2(Player, move(Piece, U, V, X, Y)),
  piece_owner_type(King, Player, king), 
  true(cell(Kx, Ky, King)),
  blocks_bishop_threat(X, Y, Tx, Ty, Kx, Ky),
  occupied_by_opponent_or_blank(X, Y, Player),
  \+threatened_with_capture(Player, X, Y, Kx, Ky, U, V).

% Legal Moves when you are:
% 1) Moving your King
% 2) NOT castling
% Does not matter whether you are currently in check, since you're moving.
legal(Player, move(King, U, V, X, Y)) :- 
  true(control(Player)), 
  piece_owner_type(King, Player, king), 
  true(cell(U, V, King)), 
  kingmove(U, V, X, Y), 
  occupied_by_opponent_or_blank(X, Y, Player), 
  \+threatened_with_capture(Player, X, Y, X, Y, U, V).

% CASTLING 
% To castle, move the king. The castle move is otherwise illegal.
% You cannot castle if you are in check. 
% You cannot castle through a threat.

% king side: white
legal(white, move(wk, e, 1, g, 1)) :- 
  true(control(white)), 
  true(cell(e, 1, wk)), 
  true(cell(f, 1, b)), 
  true(cell(g, 1, b)), 
  true(cell(h, 1, wr)), 
  \+true(piece_has_moved(wk, e, 1)), 
  \+true(piece_has_moved(wr, h, 1)), 
  \+true(check(white, _, _, _)),
  \+threatened_with_capture(white, f, 1, e, 1, f, 1),
  \+threatened_with_capture(white, g, 1, e, 1, e, 1).

% queen side: white
legal(white, move(wk, e, 1, c, 1)) :- 
  true(control(white)), 
  true(cell(a, 1, wr)), 
  true(cell(b, 1, b)), 
  true(cell(c, 1, b)), 
  true(cell(d, 1, b)), 
  true(cell(e, 1, wk)), 
  \+true(piece_has_moved(wr, a, 1)), 
  \+true(piece_has_moved(wk, e, 1)), 
  \+true(check(white, _, _, _)),
  \+threatened_with_capture(white, d, 1, e, 1, d, 1), 
  \+threatened_with_capture(white, c, 1, e, 1, c, 1).

% king side: black
legal(black, move(bk, e, 8, g, 8)) :- 
  true(control(black)), 
  true(cell(e, 8, bk)), 
  true(cell(f, 8, b)), 
  true(cell(g, 8, b)), 
  true(cell(h, 8, br)), 
  \+true(piece_has_moved(bk, e, 8)), 
  \+true(piece_has_moved(br, h, 8)), 
  \+true(check(black, _, _, _)),  
  \+threatened_with_capture(black, f, 8, e, 8, f, 8), 
  \+threatened_with_capture(black, g, 8, e, 8, g, 8).

% queen side: black
legal(black, move(bk, e, 8, c, 8)) :- 
  true(control(black)), 
  true(cell(a, 8, br)), 
  true(cell(b, 8, b)), 
  true(cell(c, 8, b)), 
  true(cell(d, 8, b)), 
  true(cell(e, 8, bk)), 
  \+true(piece_has_moved(br, a, 8)), 
  \+true(piece_has_moved(bk, e, 8)), 
  \+true(check(black, _, _, _)), 
  \+threatened_with_capture(black, d, 8, e, 8, d, 8), 
  \+threatened_with_capture(black, c, 8, e, 8, c, 8).

% NO-OPs for player not moving
legal(white, noop) :-
  true(control(black)).
legal(black, noop) :-
  true(control(white)).

/* Bug here because the test assumes X and Y are not blank (ie true has been set for hypothetical state
blocks_rook_threat(X, Y, Tx, Y, Kx, Y) :- rook_move(Tx, Y, X, Y), rook_move(X, Y, Kx, Y).
blocks_rook_threat(X, Y, X, Ty, X, Ky) :- rook_move(X, Ty, X, Y), rook_move(X, Y, X, Ky).
*/

blocks_rook_threat(X, Y, Tx, Y, Kx, Y) :- ((@<(Tx, X), @<(X, Kx)) ; (@<(Kx, X), @<(X, Tx))).
blocks_rook_threat(X, Y, X, Ty, X, Ky) :- ((@<(Ty, Y), @<(Y, Ky)) ; (@<(Ky, Y), @<(Y, Ty))).

blocks_bishop_threat(X, Y, Tx, Ty, Kx, Ky) :- 
  clear_diagonal_ne(Tx, Ty, X, Y), 
  clear_diagonal_ne(X, Y, Kx, Ky).

blocks_bishop_threat(X, Y, Tx, Ty, Kx, Ky) :- 
  clear_diagonal_ne(Kx, Ky, X, Y), 
  clear_diagonal_ne(X, Y, Tx, Ty).

blocks_bishop_threat(X, Y, Tx, Ty, Kx, Ky) :- 
  clear_diagonal_se(Tx, Ty, X, Y), 
  clear_diagonal_se(X, Y, Kx, Ky).

blocks_bishop_threat(X, Y, Tx, Ty, Kx, Ky) :- 
  clear_diagonal_se(Kx, Ky, X, Y), 
  clear_diagonal_se(X, Y, Tx, Ty).

% ADVANCE
legal2(Player, move(Piece, U, V, X, Y)) :- 
  piece_owner_type(Piece, Player, pawn), 
  true(cell(U, V, Piece)), 
  pawn_advance(U, V, X, Y, Player), 
  true(cell(X, Y, b)).

% EN PASSANT CAPTURE
legal2(Player, move(Piece, U, V, X, Y)) :- 
  piece_owner_type(Piece, Player, pawn), 
  true(cell(U, V, Piece)), 
  pawn_capture_en_passant(Player, U, V, X, Y).

% REGULAR CAPTURE
legal2(Player, move(Piece, U, V, X, Y)) :- 
  piece_owner_type(Piece, Player, pawn), 
  true(cell(U, V, Piece)), 
  pawn_capture(U, V, X, Y, Player), 
  true(cell(X, Y, Captured)), 
  piece_owner_type(Captured, Opponent, _Type), 
  opponent(Player, Opponent).

legal2(Player, move(Piece, U, V, X, Y)) :- 
  piece_owner_type(Piece, Player, queen), 
  true(cell(U, V, Piece)), 
  queen_move(U, V, X, Y).

legal2(Player, move(Piece, U, V, X, Y)) :- 
  piece_owner_type(Piece, Player, rook), 
  true(cell(U, V, Piece)), 
  rook_move(U, V, X, Y).

legal2(Player, move(Piece, U, V, X, Y)) :- 
  role(Player), 
  piece_owner_type(Piece, Player, bishop), 
  true(cell(U, V, Piece)), 
  bishop_move(U, V, X, Y).

legal2(Player, move(Piece, U, V, X, Y)) :- 
  piece_owner_type(Piece, Player, knight), 
  true(cell(U, V, Piece)), 
  knight_move(Piece, U, V, X, Y, Player).

legal2(Player, move(Piece, U, V, X, Y)) :- 
  piece_owner_type(Piece, Player, king), 
  true(cell(U, V, Piece)), 
  kingmove(U, V, X, Y).

%  King move is horizontal, vertical or diagonal
kingmove(U, V, U, Y) :- 
  adjacent(V, Y).

kingmove(U, V, X, V) :- 
  adjacent(U, X).

kingmove(U, V, X, Y) :- 
  adjacent(U, X), 
  adjacent(V, Y).

% KING_THREATENS -- Can a king owned by Attacker capture a piece at (X,Y)?
king_threatens(Attacker, X, Y) :- 
  piece_owner_type(Piece, Attacker, king), 
  true(cell(U, V, Piece)), 
  kingmove(U, V, X, Y).

%*******************************************************************************
%* LEGAL PIECE MOVE AND THREAT AXIOMS: QUEEN                                   *
%*******************************************************************************

queen_move(U, V, X, Y) :- bishop_move(U, V, X, Y).
queen_move(U, V, X, Y) :- rook_move(U, V, X, Y).

% QUEEN THREAT -- Can a queen owned by ?attacker capture a piece at (Kx,Ky)
% queen_threatens(Opponent, Tx, Ty, Kx, Ky, U, V, X, Y) 
queen_threatens(Attacker, Tx, Ty, Kx, Ky, U, V, X, Y) :- 
  piece_owner_type(Piece, Attacker, queen), 
  true(cell(Tx, Ty, Piece)), 
  queen_move_ig(Tx, Ty, Kx, Ky, U, V, X, Y).

queen_move_ig(Tx, Ty, Kx, Ky, U, V, X, Y) :- 
  bishop_move_ig(Tx, Ty, Kx, Ky, U, V, X, Y).
queen_move_ig(Tx, Ty, Kx, Ky, U, V, X, Y) :- 
  rook_move_ig(Tx, Ty, Kx, Ky, U, V, X, Y).

%*******************************************************************************
%* LEGAL PIECE MOVE AND THREAT AXIOMS: ROOK                                    *
%*******************************************************************************

rook_move(U, V, U, Y) :- 
  clear_column(U, V, Y).
rook_move(U, V, U, Y) :- 
  clear_column(U, Y, V).
rook_move(U, V, X, V) :- 
  clear_row(U, X, V).
rook_move(U, V, X, V) :- 
  clear_row(X, U, V).

clear_column(_U, V, Y) :- 
  next_rank(V, Y).

clear_column(U, V, Y) :- 
  next_rank(V, W), 
  true(cell(U, W, b)), 
  clear_column(U, W, Y).

clear_row(U, X, _V) :- 
  next_file(U, X).

clear_row(U, X, V) :- 
  next_file(U, W), 
  true(cell(W, V, b)), 
  clear_row(W, X, V).

% ROOK THREAT
% Just like rook_move except it allows two more arguments indicating a
% square that should be ignored.
% queen_threatens(Opponent, Tx, Ty, Kx, Ky, U, V, X, Y) 
rook_threatens(Attacker, Tx, Ty, Kx, Ky, U, V, X, Y) :-
  piece_owner_type(Rook, Attacker, rook), 
  true(cell(Tx, Ty, Rook)), 
  rook_move_ig(Tx, Ty, Kx, Ky, U, V, X, Y).

% Need special cases when king is moved
rook_move_ig(Tx, Ty, Kx, Ky, U, V, Kx, Ky) :- 
  clear_row_ig(Tx, Ty, Kx, Ky, U, V, Kx, Ky).
rook_move_ig(Tx, Ty, Kx, Ky, U, V, Kx, Ky) :- 
  clear_row_ig(Kx, Ky, Tx, Ty, U, V, Tx, Ty).
rook_move_ig(Tx, Ty, Kx, Ky, U, V, Kx, Ky) :- 
  clear_col_ig(Tx, Ty, Kx, Ky, U, V, Kx, Ky).
rook_move_ig(Tx, Ty, Kx, Ky, U, V, Kx, Ky) :- 
  clear_col_ig(Kx, Ky, Tx, Ty, U, V, Tx, Ty).

% Non king moves
rook_move_ig(Tx, Ty, Kx, Ky, U, V, X, Y) :-
  different_cells(Kx, Ky, X, Y),
  clear_row_ig(Tx, Ty, Kx, Ky, U, V, X, Y).
rook_move_ig(Tx, Ty, Kx, Ky, U, V, X, Y) :-
  different_cells(Kx, Ky, X, Y),
  clear_row_ig(Kx, Ky, Tx, Ty, U, V, X, Y).
rook_move_ig(Tx, Ty, Kx, Ky, U, V, X, Y) :-
  different_cells(Kx, Ky, X, Y),
  clear_col_ig(Tx, Ty, Kx, Ky, U, V, X, Y).
rook_move_ig(Tx, Ty, Kx, Ky, U, V, X, Y) :-
  different_cells(Kx, Ky, X, Y),
  clear_col_ig(Kx, Ky, Tx, Ty, U, V, X, Y).

% CLEAR_ROW_IG
% Added to prevent king moving into check
clear_row_ig(Tx, Ky, Kx, Ky, _U, _V, Kx, Ky) :- 
  next_file(Tx, Kx), !.

% base case: Tx and Kx are column neighbors
clear_row_ig(Tx, Ky, Kx, Ky, _U, _V, _X, _Y) :- 
  next_file(Tx, Kx), !.

% Blocked by new position of rook or queen
clear_row_ig(Tx, Ky, _Kx, Ky, _U, _V, X, Ky) :-
  next_file(Tx, X), !,
  false. 

% recursive case: the next square should be ignored
clear_row_ig(Tx, V, Kx, V, U, V, X, Y) :- 
  next_file(Tx, U), 
  clear_row_ig(U, V, Kx, V, U, V, X, Y).

% recursive case: the next square is blank, rest of row is clear.
clear_row_ig(Tx, Ty, Kx, Ty, U, V, X, Y) :- 
  next_file(Tx, Tx2), 
  true(cell(Tx2, Ty, b)),
  clear_row_ig(Tx2, Ty, Kx, Ty, U, V, X, Y).

% CLEAR_COL_IG
% Added to prevent king moving into check
clear_col_ig(Kx, Ty, Kx, Ky, _U, _V, Kx, Ky) :-
  next_rank(Ty, Ky), !.

% base case: ?u and ?x are adjacent rows
clear_col_ig(Kx, Ty, Kx, Ky, _U, _V, _X, _Y) :-
  next_rank(Ty, Ky), !.

% Blocked by new position of rook or queen
clear_col_ig(Kx, Ty, Kx, _Ky, _U, _V, Kx, Y) :-
  next_rank(Ty, Y), !,
  false.

% recursive case: the adjacent square should be ignored
clear_col_ig(U, Ty, U, Ky, U, V, X, Y) :- 
  next_rank(Ty, V),
  clear_col_ig(U, V, U, Ky, U, V, X, Y).

% recursive case: the adjacent square is blank, rest of column is clear.
clear_col_ig(Tx, Ty, Tx, Ky, U, V, X, Y) :- 
  next_rank(Ty, Ty2),
  true(cell(Tx, Ty2, b)), 
  clear_col_ig(Tx, Ty2, Tx, Ky, U, V, X, Y).

%*******************************************************************************
%* LEGAL PIECE MOVE AND THREAT AXIOMS: BISHOP                                  *
%*******************************************************************************

bishop_move(U, V, X, Y) :- clear_diagonal_ne(U, V, X, Y).
bishop_move(U, V, X, Y) :- clear_diagonal_ne(X, Y, U, V).
bishop_move(U, V, X, Y) :- clear_diagonal_se(U, V, X, Y).
bishop_move(U, V, X, Y) :- clear_diagonal_se(X, Y, U, V).

% CLEAR_DIAGONAL_NE: base case
clear_diagonal_ne(U, V, X, Y) :- 
  next_file(U, X), 
  next_rank(V, Y).

% CLEAR_DIAGONAL_NE: recursive case
clear_diagonal_ne(U, V, X, Y) :- 
  next_file(U, U2), 
  next_rank(V, V2), 
  true(cell(U2, V2, b)), 
  clear_diagonal_ne(U2, V2, X, Y).

% CLEAR_DIAGONAL_SE: base case
clear_diagonal_se(U, V, X, Y) :- 
  next_file(U, X), 
  next_rank(Y, V).

% CLEAR_DIAGONAL_SE: recursive case
clear_diagonal_se(U, V, X, Y) :- 
  next_file(U, U2), 
  next_rank(V2, V), 
  true(cell(U2, V2, b)), 
  clear_diagonal_se(U2, V2, X, Y).

% BISHOP THREAT
% bishop_threatens(Opponent, Tx, Ty, Kx, Ky, U, V, X, Y) 
bishop_threatens(Attacker, Tx, Ty, Kx, Ky, U, V, X, Y) :- 
  piece_owner_type(Bishop, Attacker, bishop), 
  true(cell(Tx, Ty, Bishop)), 
  bishop_move_ig(Tx, Ty, Kx, Ky, U, V, X, Y).

bishop_move_ig(Tx, Ty, Kx, Ky, U, V, X, Y) :- 
  clear_diagonal_ne_ig(Tx, Ty, Kx, Ky, U, V, X, Y).
bishop_move_ig(Tx, Ty, Kx, Ky, U, V, X, Y) :- 
  clear_diagonal_ne_ig(Kx, Ky, Tx, Ty, U, V, X, Y).
bishop_move_ig(Tx, Ty, Kx, Ky, U, V, X, Y) :- 
  clear_diagonal_se_ig(Tx, Ty, Kx, Ky, U, V, X, Y).
bishop_move_ig(Tx, Ty, Kx, Ky, U, V, X, Y) :- 
  clear_diagonal_se_ig(Kx, Ky, Tx, Ty, U, V, X, Y).

% CLEAR_DIAGONAL_NE_IG: base case
clear_diagonal_ne_ig(Tx, Ty, Kx, Ky, _U, _V, _X, _Y) :- 
  next_file(Tx, Kx), 
  next_rank(Ty, Ky).
  
% New position of bishop or queen blocks threat
clear_diagonal_ne_ig(Tx, Ty, _Kx, _Ky, _U, _V, X, Y) :-
  next_file(Tx, X),
  next_rank(Ty, Y), !,
  false.

% CLEAR_DIAGONAL_NE_IG: recursive case -- pre-move position of bishop or queen ignored
clear_diagonal_ne_ig(Tx, Ty, Kx, Ky, U, V, X, Y) :- 
  next_file(Tx, U), 
  next_rank(Ty, V), 
  clear_diagonal_ne_ig(U, V, Kx, Ky, U, V, X, Y).

% CLEAR_DIAGONAL_NE_IG: general recursive case
clear_diagonal_ne_ig(Tx, Ty, Kx, Ky, U, V, X, Y) :- 
  next_file(Tx, Tx2), 
  next_rank(Ty, Ty2), 
  true(cell(Tx2, Ty2, b)), 
  clear_diagonal_ne_ig(Tx2, Ty2, Kx, Ky, U, V, X, Y).

% CLEAR_DIAGONAL_SE_IG: base case
clear_diagonal_se_ig(Tx, Ty, Kx, Ky, _U, _V, _X, _Y) :- 
  next_file(Tx, Kx), 
  next_rank(Ky, Ty).

% New position of bishop or queen blocks threat
clear_diagonal_se_ig(Tx, Ty, _Kx, _Ky, _U, _V, X, Y) :- 
  next_file(Tx, X), 
  next_rank(Y, Ty), !,
  false.

% CLEAR_DIAGONAL_SE_IG: recursive case -- (x,y) ignored
clear_diagonal_se_ig(Tx, Ty, Kx, Ky, U, V, X, Y) :- 
  next_file(Tx, U), 
  next_rank(V, Ty), 
  clear_diagonal_se_ig(U, V, Kx, Ky, U, V, X, Y).

% CLEAR_DIAGONAL_SE_IG: general recursive
clear_diagonal_se_ig(Tx, Ty, Kx, Ky, U, V, X, Y) :- 
  next_file(Tx, Tx2), next_rank(Ty2, Ty), 
  true(cell(Tx2, Ty2, b)), 
  clear_diagonal_se_ig(Tx2, Ty2, Kx, Ky, U, V, X, Y).

%*******************************************************************************
%* LEGAL PIECE MOVE AND THREAT AXIOMS: KNIGHT                                  *
%*******************************************************************************

% Up two + over one in any direction
knight_move(Piece, U, V, X, Y, Owner) :- 
  piece_owner_type(Piece, Owner, knight), 
  adjacent_two(V, Y), 
  adjacent(U, X).

% Up one + over two in any direction
knight_move(Piece, U, V, X, Y, Owner) :- 
  piece_owner_type(Piece, Owner, knight), 
  adjacent_two(U, X), 
  adjacent(V, Y).

% KNIGHT_THREATENS -- Can a knight owned by ?attacker capture a piece at (x,y)
knight_threatens(Attacker, X, Y) :- 
  piece_owner_type(Piece, Attacker, knight), 
  true(cell(U, V, Piece)), 
  knight_move(Piece, U, V, X, Y, Attacker).

%*******************************************************************************
%* LEGAL PIECE MOVE AND THREAT AXIOMS: PAWN                                    *
%*******************************************************************************


% white pawns advance up the rows
pawn_advance(U, V, U, Y, white) :- 
  next_rank(V, Y).

pawn_advance(U, 2, U, 4, white) :-
  true(cell(U, 3, b)).

% blacks pawns advance down the rows
pawn_advance(U, V, U, Y, black) :- 
  next_rank(Y, V).

pawn_advance(U, 7, U, 5, black) :-
  true(cell(U, 6, b)).

% Can a pawn at (u,v) owned by ?player capture a piece at (x,y)?
pawn_capture(U, V, X, Y, white) :- 
  next_rank(V, Y), 
  (next_file(U, X) ; next_file(X, U)).

pawn_capture(U, V, X, Y, black) :- 
  next_rank(Y, V), 
  (next_file(U, X) ; next_file(X, U)).

% IS EN PASSSANT CAPTURE
% true iff moving the piece from the source to the destination
% cell constitutes an en passant capture
pawn_capture_en_passant(white, X1, 5, X2, 6) :- 
  true(cell(X1, 5, wp)), 
  true(pawn_moved_two(bp, X2)), 
  pawn_capture(X1, 5, X2, 6, white).

pawn_capture_en_passant(black, X1, 4, X2, 3) :- 
  true(cell(X1, 4, bp)), 
  true(pawn_moved_two(wp, X2)), 
  pawn_capture(X1, 4, X2, 3, black).

%*******************************************************************************
%* THREAT AXIOMS                                                               *
%* True if a piece owned by ?owner at (x,y) could be captured.                 *
%* (ignorex,ignorey) is a square to be ignored, usually because it is          *
%* occupied by a piece that is going to be moved.                              *
%*******************************************************************************

% threatened even though ?owner capturing at ?cx ?cy
% threatened-with-capture is like threatened, but it won't consider threats 
% originating from ?x ?y, because that piece is being captured.

% Pawn
threatened_with_capture(Owner, X, Y, Kx, Ky, _U, _V) :- 
  opponent(Owner, Opponent), 
  piece_owner_type(Opiece, Opponent, pawn), 
  true(cell(Tx, Ty, Opiece)), 
  different_cells(X, Y, Tx, Ty), 
  pawn_capture(Tx, Ty, Kx, Ky, Opponent).

% Bishop, Rook, or Queen
threatened_with_capture(Owner, X, Y, Kx, Ky, U, V) :-
  opponent(Owner, Opponent), 
  ( bishop_threatens(Opponent, Tx, Ty, Kx, Ky, U, V, X, Y) 
  ; rook_threatens(Opponent, Tx, Ty, Kx, Ky, U, V, X, Y) 
  ; queen_threatens(Opponent, Tx, Ty, Kx, Ky, U, V, X, Y)
  ), 
  different_cells(X, Y, Tx, Ty).

% Knight
threatened_with_capture(Owner, X, Y, Kx, Ky, _U, _V) :- 
  opponent(Owner, Opponent), 
  piece_owner_type(Piece, Opponent, knight), 
  true(cell(Tx, Ty, Piece)), 
  different_cells(X, Y, Tx, Ty), 
  knight_move(Piece, Tx, Ty, Kx, Ky, Opponent).

% King attacking King
threatened_with_capture(Owner, _X, _Y, Kx, Ky, _U, _V) :-
  opponent(Owner, Opponent), 
  king_threatens(Opponent, Kx, Ky).

%*******************************************************************************
% AUXILIARY PREDICATES                                                         *
%*******************************************************************************

% DIFFERENT CELLS
% True iff ?x1 ?y1 is a different cell from ?x2 ?y2
adjacent(X1, X2) :- next_file(X1, X2).
adjacent(X1, X2) :- next_file(X2, X1).
adjacent(Y1, Y2) :- next_rank(Y1, Y2).
adjacent(Y1, Y2) :- next_rank(Y2, Y1).

adjacent_two(A, B) :- 
  adjacent(A, Mid), 
  adjacent(Mid, B), 
  dif(A, B).

different_cells(X1, _Y1, X2, _Y2) :- 
  X1 \== X2.

different_cells(_X1, Y1, _X2, Y2) :- 
  Y1 \== Y2.

has_legal_move(Player) :- legal(Player, move(_Piece, _U, _V, _X, _Y)).

occupied_by_opponent(X, Y, Player) :- 
  true(cell(X, Y, Piece)), 
  opponent(Player, Opponent), 
  piece_owner_type(Piece, Opponent, _Type).

occupied_by_player(X, Y, Player) :- 
  true(cell(X, Y, Piece)), 
  piece_owner_type(Piece, Player, _Type).

occupied_by_opponent_or_blank(X, Y, Player) :- 
  true(cell(X, Y, b)), 
  role(Player).

occupied_by_opponent_or_blank(X, Y, Player) :- 
  occupied_by_opponent(X, Y, Player).

%*******************************************************************************
%* GAME TERMINATION CONDITIONS                                                 *
%*******************************************************************************

checkmate(Player) :- 
  true(control(Player)), 
  true(check(Player, _Ptype, _X, _Y)), 
  stuck(Player).

stalemate :- 
  true(control(Player)), 
  \+true(check(Player, _Ptype, _X, _Y)), 
  stuck(Player).

stuck(Player) :- 
  role(Player), 
  \+has_legal_move(Player).

%*******************************************************************************
%* GOALS AND TERMINAL STATES                                                   *
%* Player gets 100 for checkmating the opponent,                               *
%* 50 for stalemating and 0 for being checkmated.                              *
%*******************************************************************************
terminal :- true(control(Player)), stuck(Player).

% Missing in original
terminal :- checkmate(_).
terminal :- stalemate.

goal(white, 100) :- checkmate(black).
goal(white, 50) :- stalemate.
goal(white, 0) :- checkmate(white).
goal(black, 100) :- checkmate(white).
goal(black, 50) :- stalemate.
goal(black, 0) :- checkmate(black).

%*******************************************************************************
%* GROUND FACTS                                                                *
%*******************************************************************************

% PLAYER OPPONENTS
opponent(white, black).
opponent(black, white).

% PIECE OWNERSHIP AND TYPE 
piece_owner_type(wk, white, king).
piece_owner_type(wq, white, queen).
piece_owner_type(wr, white, rook).
piece_owner_type(wb, white, bishop).
piece_owner_type(wn, white, knight).
piece_owner_type(wp, white, pawn).

piece_owner_type(bk, black, king).
piece_owner_type(bq, black, queen).
piece_owner_type(br, black, rook).
piece_owner_type(bb, black, bishop).
piece_owner_type(bn, black, knight).
piece_owner_type(bp, black, pawn).

% IS CASTLE CELL
% True iff the cell designated by the second pair of arguments is one of the 
% castle cells in the same corner of the board as the first pair of arguments.
is_castle_cell(g, 1, e, 1).
is_castle_cell(g, 1, f, 1).
is_castle_cell(g, 1, g, 1).
is_castle_cell(g, 1, h, 1).

is_castle_cell(c, 1, a, 1).
is_castle_cell(c, 1, b, 1).
is_castle_cell(c, 1, c, 1).
is_castle_cell(c, 1, d, 1).
is_castle_cell(c, 1, e, 1).

is_castle_cell(g, 8, e, 8).
is_castle_cell(g, 8, f, 8).
is_castle_cell(g, 8, g, 8).
is_castle_cell(g, 8, h, 8).

is_castle_cell(c, 8, a, 8).
is_castle_cell(c, 8, b, 8).
is_castle_cell(c, 8, c, 8).
is_castle_cell(c, 8, d, 8).
is_castle_cell(c, 8, e, 8).

% IS CASTLE MOVE
% True iff moving the piece from source to destination constitutes a castle
is_castle_move(wk, e, 1, c, 1).
is_castle_move(wk, e, 1, g, 1).
is_castle_move(bk, e, 8, c, 8).
is_castle_move(bk, e, 8, g, 8).

% BOARD TOPOLOGY
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

% need to remember to add this to each set of rules
utility(Role, Value) :- 
  findall(Base, true(Base), State),
  piece_count(Role, State, 0, Count),
  move_bonus(Role, Bonus),
  Value is Count + Bonus.

piece_count(_Role, [], Value, Value) :- !.

piece_count(white, [cell(_,_,wp)|State], Value, Acc) :-
  Value1 is Value + 2,
  piece_count(white, State, Value1, Acc).

piece_count(black, [cell(_,_,bp)|State], Value, Acc) :-
  Value1 is Value + 2,
  piece_count(black, State, Value1, Acc).

piece_count(white, [cell(_,_,wn)|State], Value, Acc) :-
  Value1 is Value + 4,
  piece_count(white, State, Value1, Acc).

piece_count(black, [cell(_,_,bn)|State], Value, Acc) :-
  Value1 is Value + 4,
  piece_count(black, State, Value1, Acc).

piece_count(white, [cell(_,_,wb)|State], Value, Acc) :-
  Value1 is Value + 4,
  piece_count(white, State, Value1, Acc).

piece_count(black, [cell(_,_,bb)|State], Value, Acc) :-
  Value1 is Value + 4,
  piece_count(black, State, Value1, Acc).

piece_count(white, [cell(_,_,wr)|State], Value, Acc) :-
  Value1 is Value + 6,
  piece_count(white, State, Value1, Acc).

piece_count(black, [cell(_,_,br)|State], Value, Acc) :-
  Value1 is Value + 6,
  piece_count(black, State, Value1, Acc).

piece_count(white, [cell(_,_,wq)|State], Value, Acc) :-
  Value1 is Value + 10,
  piece_count(white, State, Value1, Acc).

piece_count(black, [cell(_,_,bq)|State], Value, Acc) :-
  Value1 is Value + 10,
  piece_count(black, State, Value1, Acc).

piece_count(white, [cell(_,_,Piece)|State], Value, Acc) :-
  memberchk(Piece, [b, bp, bn, bb, br, bq, bk, wk]),
  piece_count(white, State, Value, Acc).

piece_count(black, [cell(_,_,Piece)|State], Value, Acc) :-
  memberchk(Piece, [b, wp, wn, wb, wr, wq, wk, bk]),
  piece_count(black, State, Value, Acc).

piece_count(white, [check(black,_,_,_)|State], Value, Acc) :-
  Value1 is Value + 1,
  piece_count(white, State, Value1, Acc).

piece_count(black, [check(white,_,_,_)|State], Value, Acc) :-
  Value1 is Value + 1,
  piece_count(black, State, Value1, Acc).

piece_count(white, [check(white,_,_,_)|State], Value, Acc) :-
  Value1 is Value - 1,
  piece_count(white, State, Value1, Acc).

piece_count(black, [check(black,_,_,_)|State], Value, Acc) :-
  Value1 is Value - 1,
  piece_count(black, State, Value1, Acc).


piece_count(white, [piece_has_moved(wk,_,_)|State], Value, Acc) :-
  Value1 is Value - 1,
  piece_count(white, State, Value1, Acc).

piece_count(black, [piece_has_moved(bk,_,_)|State], Value, Acc) :-
  Value1 is Value - 1,
  piece_count(black, State, Value1, Acc).

piece_count(white, [piece_has_moved(wr,_,_)|State], Value, Acc) :-
  Value1 is Value - 1,
  piece_count(white, State, Value1, Acc).

piece_count(black, [piece_has_moved(br,_,_)|State], Value, Acc) :-
  Value1 is Value - 1,
  piece_count(black, State, Value1, Acc).

piece_count(white, [piece_has_moved(Piece,_,_)|State], Value, Acc) :-
  memberchk(Piece, [bk, br]),
  piece_count(white, State, Value, Acc).

piece_count(black, [piece_has_moved(Piece,_,_)|State], Value, Acc) :-
  memberchk(Piece, [wk, wr]),
  piece_count(black, State, Value, Acc).

piece_count(white, [pawn_moved_two(wp, _)|State], Value, Acc) :-
  % Value1 is Value + 1,
  piece_count(white, State, Value, Acc).

piece_count(black, [pawn_moved_two(bp, _)|State], Value, Acc) :-
  % Value1 is Value + 1,
  piece_count(black, State, Value, Acc).

piece_count(white, [pawn_moved_two(bp, _)|State], Value, Acc) :-
  piece_count(white, State, Value, Acc).

piece_count(black, [pawn_moved_two(wp, _)|State], Value, Acc) :-
  piece_count(black, State, Value, Acc).

piece_count(Player, [control(_)|State], Value, Acc) :-
  piece_count(Player, State, Value, Acc).

move_bonus(white, Bonus) :-
  (  ( does(white, move(wk, e, 1, g, 1)) ; does(white, move(wk, e, 1, c, 1)) )
  -> Bonus = 1
  ;  Bonus = 0
  ).

move_bonus(black, Bonus) :-
  (  ( does(black, move(bk, e, 8, g, 8)) ; does(black, move(bk, e, 8, c, 8)) )
  -> Bonus = 1
  ;  Bonus = 0
  ).

