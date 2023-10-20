:- module(ggp, 
  [ get_start/2
  , update_true/2
  , findedges/2
  , terminal_utility/5
  ]).

get_start(Game, Start) :-
  findall(Init, Game:init(Init), Unsorted),
  sort(Unsorted, Start).

%% update_true(+Game, +State) is det
% needed put true(Proposition) in clausal store
update_true(Game, State) :-
  retractall(Game:true(_)), 
  forall(member(Base, State), assertz(Game:true(Base))).

%% update_does(+Game, +DoesList) is det
% needed to put does(Role, Move) in clausal store
update_does(Game, DoesList) :-
  retractall(Game:does(_,_)),
  forall(member(does(Role, Move), DoesList), assertz(Game:does(Role, Move))).

%% cartprod(+S, L) is det
% S is a list of lists, so finds cartesian product for any number of players  
cartprod(S, L) :-
  findall(R, cart(S, R), L).

cart([], []).
cart([[A | _] | T], [A | R]) :-
  cart(T, R).
cart([[_ | B] | T], R) :-
  cart([B | T], R).

findlegals(Game, Role, Legals) :-
  findall(does(Role, Legal), Game:legal(Role, Legal), Unsorted),
  sort(Unsorted, Legals).

%% findedges(+Game, -Edges) is det
% Produces a cartesian product of DoesList for findnext 
findedges(Game, Edges) :-
  findall(Role, Game:role(Role), Roles),
  maplist(findlegals(Game), Roles, Legals),
  cartprod(Legals, Edges).

% Assumes this is not a terminal state, besides that update_true(Game, State) has been called
findnext(Game, DoesList, Next) :-
  update_does(Game, DoesList),
  findall(Proposition, Game:next(Proposition), Unsorted),
  sort(Unsorted, Next).

findgoals(Game, Goals) :-
  findall(goal(Role, Value), (Game:role(Role), Game:goal(Role, Value)), Unsorted),
  sort(Unsorted, Goals).

findutility(Game, DoesList, State, Utility) :-
  update_true(Game, State),
  update_does(Game, DoesList),
  findall(goal(Role, Value), (Game:role(Role), Game:utility(Role, Value)), Utility).

terminal_utility(Game, Move, Next, Terminal, Utility) :-
  ggp:update_true(Game, Next),
  ( Game:terminal
  -> ggp:findgoals(Game, Utility), Terminal = true
  ;  ggp:findutility(Game, Move, Next, Utility), Terminal = false
  ).
