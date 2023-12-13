
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

spring('#') --> "#".
spring('.') --> ".".
spring('?') --> "?".

springs([S|T]) --> spring(S), springs(T), !.
springs([]) --> [].

int_list([H|T]) --> integer(H), ",", !, int_list(T).
int_list([I]) --> integer(I).

inp_list([[Springs, Ranges]|Rest]) --> springs(Springs), " ", int_list(Ranges), ( "\n" ; []), inp_list(Rest), !.
inp_list([]) --> [].

inp1(Lst) :- phrase_from_file(inp_list(Lst), "inputs/day12.test").
realinp(Lst) :- phrase_from_file(inp_list(Lst), "inputs/day12.inp").

rev_member(L, X) :- member(X, L).

solve_part1([], [], []).
solve_part1(['.'|ST], Ns, ['.'|RT]) :- solve_part1(ST, Ns, RT).
solve_part1(L, [N], R) :-
  length(L, N),
  length(R, N),
  maplist(rev_member(['#', '?']), L),
  maplist(=('#'), R), !.
solve_part1(['#'|S], [N|Ns], R) :-
  length(['#'|PrefL], N),
  append(PrefL, [C|ST], S),
  maplist(rev_member(['#', '?']), PrefL),
  member(C, ['?', '.']),

  length(PrefR, N),
  maplist(=('#'), PrefR),
  append(PrefR, ['.'|RT], R),

  solve_part1(['.'|ST], Ns, ['.'|RT]).
solve_part1(['?'|ST], Ns, R) :- solve_part1(['#'|ST], Ns, R); solve_part1(['.'|ST], Ns, R).

part1_helper([S, N], C) :- aggregate_all(count, solve_part1(S, N, _), C).

part_1(File, N) :-
  phrase_from_file(inp_list(L), File),
  maplist(part1_helper, L, Ns),
  sum_list(Ns, N).
