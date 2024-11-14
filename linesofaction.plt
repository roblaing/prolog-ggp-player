:- begin_tests(linesofaction).
:- use_module([
  "/usr/local/lib/swipl/ggp.pl",
  "/usr/local/lib/swipl/terms_array.pl",
  "/usr/local/games/linesofaction.pl"
]).

test(start, Start == [control(black),
  cell(a,2,white),cell(a,3,white),cell(a,4,white),cell(a,5,white),cell(a,6,white),cell(a,7,white),
  cell(b,1,black),cell(b,8,black),
  cell(c,1,black),cell(c,8,black),
  cell(d,1,black),cell(d,8,black),
  cell(e,1,black),cell(e,8,black),
  cell(f,1,black),cell(f,8,black),
  cell(g,1,black),cell(g,8,black),
  cell(h,2,white),cell(h,3,white),cell(h,4,white),cell(h,5,white),cell(h,6,white),cell(h,7,white)]) :-
  get_start(linesofaction, Start).

test(startmoves_black, Legals == [
  does(black,move(b,1,b,3)),does(black,move(b,1,d,3)),does(black,move(b,1,h,1)),does(black,move(b,8,b,6)),does(black,move(b,8,d,6)),does(black,move(b,8,h,8)),
  does(black,move(c,1,a,3)),does(black,move(c,1,c,3)),
  does(black,move(c,1,e,3)),does(black,move(c,8,a,6)),does(black,move(c,8,c,6)),does(black,move(c,8,e,6)),
  does(black,move(d,1,b,3)),does(black,move(d,1,d,3)),does(black,move(d,1,f,3)),does(black,move(d,8,b,6)),does(black,move(d,8,d,6)),does(black,move(d,8,f,6)),
  does(black,move(e,1,c,3)),does(black,move(e,1,e,3)),does(black,move(e,1,g,3)),does(black,move(e,8,c,6)),does(black,move(e,8,e,6)),does(black,move(e,8,g,6)),
  does(black,move(f,1,d,3)),does(black,move(f,1,f,3)),does(black,move(f,1,h,3)),does(black,move(f,8,d,6)),does(black,move(f,8,f,6)),does(black,move(f,8,h,6)),
  does(black,move(g,1,a,1)),does(black,move(g,1,e,3)),does(black,move(g,1,g,3)),does(black,move(g,8,a,8)),does(black,move(g,8,e,6)),does(black,move(g,8,g,6))]) :-
  get_start(linesofaction, Start),
  update_true(linesofaction, Start),
  ggp:findlegals(linesofaction, black, Legals).

test(startmoves_white, Legals == [does(white,noop)]) :-
  get_start(linesofaction, Start),
  update_true(linesofaction, Start),
  ggp:findlegals(linesofaction, white, Legals).

test(cant_jump_enemy, Legals = [
   does(black,move(b,1,b,3)),
   does(black,move(b,1,e,1)),
   does(black,move(c,8,a,6)),does(black,move(c,8,c,7)),does(black,move(c,8,g,8)),does(black,move(d,1,a,1)),does(black,move(d,1,b,3)),does(black,move(d,1,d,5)),does(black,move(d,1,e,2)),does(black,move(d,3,a,3)),does(black,move(d,3,b,5)),does(black,move(d,3,d,7)),does(black,move(d,3,f,1)),does(black,move(d,4,a,7)),does(black,move(d,4,b,2)),does(black,move(d,4,d,8)),does(black,move(d,4,f,6)),does(black,move(d,6,a,6)),does(black,move(d,6,d,2)),does(black,move(d,6,g,3)),does(black,move(d,6,g,6)),does(black,move(e,5,b,8)),does(black,move(e,5,c,3)),does(black,move(e,5,c,5)),does(black,move(e,5,g,7)),does(black,move(e,5,h,2)),does(black,move(e,6,b,6)),does(black,move(e,6,c,4)),does(black,move(e,6,g,8)),does(black,move(e,6,h,6)),does(black,move(e,8,a,8)),does(black,move(e,8,c,6)),does(black,move(e,8,f,7)),does(black,move(f,4,c,7)),does(black,move(f,4,f,1)),does(black,move(f,4,h,6)),does(black,move(f,8,b,8)),does(black,move(f,8,c,5)),does(black,move(f,8,f,5)),does(black,move(f,8,g,7))
  ]) :-
  JSON = [["control","black"],
["cell","a",2,"white"],
["cell","a",4,"white"],
["cell","a",6,"white"],
["cell","a",7,"white"],
["cell","a",8,"white"],
["cell","b",1,"black"],
["cell","b",4,"white"],
["cell","c",8,"black"],
["cell","d",1,"black"],
["cell","d",3,"black"],
["cell","d",4,"black"],
["cell","d",6,"black"],
["cell","e",1,"white"],
["cell","e",3,"white"],
["cell","e",4,"white"],
["cell","e",5,"black"],
["cell","e",6,"black"],
["cell","e",8,"black"],
["cell","f",4,"black"],
["cell","f",5,"white"],
["cell","f",8,"black"],
["cell","h",3,"white"],
["cell","h",7,"white"]],
  terms_array:array_terms(JSON, State),
  update_true(linesofaction, State),
  ggp:findlegals(linesofaction, black, Legals).
  

test(max_cluster_size_black, Size = 6) :-
  State = [control(black),
  cell(a,2,white),cell(a,4,white),cell(a,6,white),cell(a,7,white),cell(a,8,white),
  cell(b,1,black),cell(b,4,white),
  cell(c,8,black),
  cell(d,1,black),cell(d,3,black),cell(d,4,black),cell(d,6,black),
  cell(e,1,white),cell(e,3,white),cell(e,4,white),cell(e,5,black),cell(e,6,black),cell(e,8,black),
  cell(f,4,black),cell(f,5,white),cell(f,8,black),
  cell(h,3,white),cell(h,7,white)],
  update_true(linesofaction, State),
  linesofaction:max_cluster_size(black, Size).

test(max_cluster_size_white, Size = 3) :-
  State = [control(black),
  cell(a,2,white),cell(a,4,white),cell(a,6,white),cell(a,7,white),cell(a,8,white),
  cell(b,1,black),cell(b,4,white),
  cell(c,8,black),
  cell(d,1,black),cell(d,3,black),cell(d,4,black),cell(d,6,black),
  cell(e,1,white),cell(e,3,white),cell(e,4,white),cell(e,5,black),cell(e,6,black),cell(e,8,black),
  cell(f,4,black),cell(f,5,white),cell(f,8,black),
  cell(h,3,white),cell(h,7,white)],
  update_true(linesofaction, State),
  linesofaction:max_cluster_size(white, Size).

test(centrality_black, Sum = 8) :-
  State = [control(black),
  cell(a,2,white),cell(a,4,white),cell(a,6,white),cell(a,7,white),cell(a,8,white),
  cell(b,1,black),cell(b,4,white),
  cell(c,8,black),
  cell(d,1,black),cell(d,3,black),cell(d,4,black),cell(d,6,black),
  cell(e,1,white),cell(e,3,white),cell(e,4,white),cell(e,5,black),cell(e,6,black),cell(e,8,black),
  cell(f,4,black),cell(f,5,white),cell(f,8,black),
  cell(h,3,white),cell(h,7,white)],
  update_true(linesofaction, State),
  linesofaction:centrality_sum(black, Sum).

test(centrality_white, Sum = 4) :-
  State = [control(black),
  cell(a,2,white),cell(a,4,white),cell(a,6,white),cell(a,7,white),cell(a,8,white),
  cell(b,1,black),cell(b,4,white),
  cell(c,8,black),
  cell(d,1,black),cell(d,3,black),cell(d,4,black),cell(d,6,black),
  cell(e,1,white),cell(e,3,white),cell(e,4,white),cell(e,5,black),cell(e,6,black),cell(e,8,black),
  cell(f,4,black),cell(f,5,white),cell(f,8,black),
  cell(h,3,white),cell(h,7,white)],
  update_true(linesofaction, State),
  linesofaction:centrality_sum(white, Sum).

test(utility_black, Utility = 64) :-
  State = [control(black),
  cell(a,2,white),cell(a,4,white),cell(a,6,white),cell(a,7,white),cell(a,8,white),
  cell(b,1,black),cell(b,4,white),
  cell(c,8,black),
  cell(d,1,black),cell(d,3,black),cell(d,4,black),cell(d,6,black),
  cell(e,1,white),cell(e,3,white),cell(e,4,white),cell(e,5,black),cell(e,6,black),cell(e,8,black),
  cell(f,4,black),cell(f,5,white),cell(f,8,black),
  cell(h,3,white),cell(h,7,white)],
  update_true(linesofaction, State),
  linesofaction:utility(black, Utility).

test(utility_white, Utility = 57) :-
  State = [control(black),
  cell(a,2,white),cell(a,4,white),cell(a,6,white),cell(a,7,white),cell(a,8,white),
  cell(b,1,black),cell(b,4,white),
  cell(c,8,black),
  cell(d,1,black),cell(d,3,black),cell(d,4,black),cell(d,6,black),
  cell(e,1,white),cell(e,3,white),cell(e,4,white),cell(e,5,black),cell(e,6,black),cell(e,8,black),
  cell(f,4,black),cell(f,5,white),cell(f,8,black),
  cell(h,3,white),cell(h,7,white)],
  update_true(linesofaction, State),
  linesofaction:utility(white, Utility).

test(neither_side_can_move, Legals = [does(black,move(b,1,a,2)),does(black,move(b,1,b,2)),does(black,move(b,1,e,1)),does(black,move(c,3,b,4)),does(black,move(c,3,c,1)),does(black,move(c,3,c,5)),does(black,move(c,3,d,2)),does(black,move(c,8,b,7)),does(black,move(c,8,b,8)),does(black,move(c,8,c,6)),does(black,move(c,8,d,8)),does(black,move(e,6,g,8)),does(black,move(e,6,h,3)),does(black,move(e,6,h,6)),does(black,move(f,1,c,1)),does(black,move(f,1,d,3)),does(black,move(f,1,f,4)),does(black,move(f,1,g,2)),does(black,move(f,5,f,8)),does(black,move(g,1,d,1)),does(black,move(g,1,g,3)),does(black,move(g,1,h,2)),does(black,move(g,6,d,6)),does(black,move(g,6,e,8)),does(black,move(g,6,g,4)),does(black,move(g,6,g,8))]) :-
  JSON =  [["control","black"],["cell","a",4,"white"],["cell","b",1,"black"],["cell","c",3,"black"],["cell","c",8,"black"],["cell","d",3,"white"],["cell","d",4,"white"],["cell","d",5,"white"],["cell","d",6,"white"],["cell","e",3,"white"],["cell","e",4,"white"],["cell","e",5,"white"],["cell","e",6,"black"],["cell","f",1,"black"],["cell","f",4,"white"],["cell","f",5,"black"],["cell","g",1,"black"],["cell","g",6,"black"],["cell","h",5,"white"],["cell","h",7,"white"]],
  terms_array:array_terms(JSON, State),
  update_true(linesofaction, State),
  ggp:findlegals(linesofaction, black, Legals).

:- end_tests(linesofaction).