:- module(terms_array, [ terms_array/2
                       , terms_dict/2
                       , array_terms/2
                       ]).

/** <module> One-directional translation of list of Prolog facts to a Json nested array.

@author Robert Laing
@license GPL
**/

/**
 * terms_array(+Terms:list(term), -List(text, number, or list)) is det
 *
 * one-directional conversion between states represented in Json and Prolog
 *
 * ```prolog
 terms_array(
 [control(white),
 cell(1,1,b),cell(1,2,b),cell(1,3,b),
 cell(2,1,b),cell(2,2,b),cell(2,3,b),
 cell(3,1,b),cell(3,2,b),cell(3,3,b)],
 Json).

Json = 
[["control","white"],
["cell",1,1,"b"],["cell",1,2,"b"],["cell",1,3,"b"],
["cell",2,1,"b"],["cell",2,2,"b"],["cell",2,3,"b"],
["cell",3,1,"b"],["cell",3,2,"b"],["cell",3,3,"b"]]

terms_array(
[[does(white,mark(1,1)),does(black,noop)],[does(white,mark(1,2)),does(black,noop)],[does(white,mark(1,3)),does(black,noop)],[does(white,mark(2,1)),does(black,noop)],[does(white,mark(2,2)),does(black,noop)],[does(white,mark(2,3)),does(black,noop)],[does(white,mark(3,1)),does(black,noop)],[does(white,mark(3,2)),does(black,noop)],[does(white,mark(3,3)),does(black,noop)]], Json).

 **/


terms_array(Terms, Array) :-
  maplist(terms_array_, Terms, Array).

terms_array_(Number, Number) :-
  number(Number), !.

terms_array_(Atom, String) :-
  atom(Atom), !,
  atom_string(Atom, String).

terms_array_(Compound, Array) :-
  compound(Compound), \+ is_list(Compound), !,
  Compound =.. Lst,
  maplist(terms_array_, Lst, Array).

terms_array_(List, Array) :-
  is_list(List), !,
  maplist(terms_array_, List, Array).

/**
 * terms_dict(+Terms:list(term), Dict for Json Object) is det
 *
 * ```prolog
 * terms_dict([goal(white, 50), goal(black, 50)], Dict).
 * Dict = dict{black:50, white:50}.
 *
 * terms_dict([does(white,mark(1,1)),does(black,noop)], Dict).
 * Dict = dict{black:"noop", white:[mark, 1, 1]}.
 * ```
 **/

terms_dict(PredLst, Dict) :-
  maplist(=.., PredLst, Terms),
  terms_dict_(Terms, dict{}, _, Dict).

terms_dict_([], Dict, _, Dict).

terms_dict_([[_, Key, Value]|Terms], DictIn, DictOut, Acc) :-
  terms_array_(Value, Val),
  put_dict(Key, DictIn, Val, DictOut),
  terms_dict_(Terms, DictOut, _, Acc).

/**
 * array_terms(+Json Array, -Prolog List of Terms) is det
 * 
 * Assumes no nested arrays

array_terms([["control","white"],
["cell",1,1,"b"],["cell",1,2,"b"],["cell",1,3,"b"],
["cell",2,1,"b"],["cell",2,2,"b"],["cell",2,3,"b"],
["cell",3,1,"b"],["cell",3,2,"b"],["cell",3,3,"b"]], Terms).

Terms = [control(white),
 cell(1,1,b),cell(1,2,b),cell(1,3,b),
 cell(2,1,b),cell(2,2,b),cell(2,3,b),
 cell(3,1,b),cell(3,2,b),cell(3,3,b)]

**/
 
array_terms(Array, Terms) :-
  maplist(lst_pred, Array, Terms).

lst_pred(Lst, Term) :-
  maplist(trstr, Lst, Lst1),
  Term =.. Lst1.

trstr(String, Atom) :-
  string(String), !,
  atom_string(Atom, String).

trstr(Number, Number) :-
  number(Number).


