:- module(message_broker, [message_broker/2]).
:- use_module([ terms_array
              , ggp
              , chess
              , chinese_checkers2
              , draughts
              , tictactoe
              ]).

message_broker(DictIn, DictOut) :-
  string_to_atom(DictIn.game, Game),
  action(DictIn.action, Game, DictIn, DictOut).

action("generate_node", Game, DictIn, DictOut) :-
  terms_array:array_terms(DictIn.state, State),
  ggp:update_true(Game, State),
  (  Game:terminal
  -> terminal_node(Game, DictIn, DictOut)
  ;  parent_node(Game, DictIn, DictOut)
  ).

action("new_game", Game, _DictIn, DictOut) :-
  ggp:get_start(Game, Init),
  terms_array:terms_array(Init, JInit),
  action("generate_node", Game, json{state: JInit}, DictOut).

action("get_roles", Game, _DictIn, DictOut) :-
  findall(Role, Game:role(Role), Unsorted),
  sort(Unsorted, ARoles),
  maplist(atom_string, ARoles, SRoles),
  DictOut = json{roles: SRoles}.

terminal_node(Game, DictIn, DictOut) :-
  ggp:findgoals(Game, Goals),
  terms_array:terms_dict(Goals, JGoals),
  DictOut = json{ game: Game
                , state: DictIn.state
                , terminal: true
                , goal: JGoals
                }.

parent_node(Game, DictIn, DictOut) :-
  ggp:findedges(Game, Moves),
  maplist(ggp:findnext(Game), Moves, Nexts),
  maplist(ggp:terminal_utility(Game), Moves, Nexts, Terminals, Utilities),
  maplist(terms_array:terms_dict, Moves, JMoves),
  maplist(terms_array:terms_array, Nexts, JNexts),
  maplist(terms_array:terms_dict, Utilities, JUtilities),
  DictOut = json{ game: Game
                , state: DictIn.state
                , terminal: false
                , legals: JMoves
                , nexts: JNexts
                , utilities: JUtilities
                , terminals: Terminals
                }.


