/**
 * coursework yo
 *
 * So we have a little game where there's N piles of stones, of any quantity.
 * Two players take it in turns to take a number of stones form a pile, 
 * or an equal number of stones from two piles. The player to pick up the last
 * stone is the loser
 */

/**
 * initial test case: 3 piles of 3, 2, and 1 stones respectively
 */

% prints elements in a list
print_state([]).
print_state([P|S]) :-
  write(P),
  nl,
  print_state(S).

list_popfront([X|Tail], X, Tail).
list_pushfront(X, L, [X|L]).

/**
 * Question 1
 * Produce all possible states, S2, from the current state, S1
 * e.g., for state S1 = [3, 2, 1]
 *   S2 = [
 *     [2, 2, 1], [1, 2, 1], [2, 1], % first heap only
 *     [3, 1, 1], [3, 1], % second heap only
 *     [3, 2], % third heap only
 *     [2, 1, 1], [1, 1], % first and second heap (2)
 *     [2, 2], % first and third heap (1)
 *     [3, 1], % second and third heap (1)
 *   ]
 */
rempve([F|O], O, N):-
  N = F.
remove([F|O], [NF|O], N) :-
  N < F,
  NF is F-N.
remove([F|O], [F|NO], N) :-
  remove(O, NO, N).

take_all(S, S1) :-
  list_popfront(S, _, S1).

take_n([S|T], [T], N) :-
  S = N.

take_some_doit(S, N, S1) :-
  list_pushfront(N, S, S1).
take_some(S, S2) :-
  list_popfront(S, E, S1),
  E1 is E-1,
  between(1, E1, X),
  (
    take_some_doit(S1, X, S2)
  ).
  
/**
 * Question 2
 * Return true if S is a winning position
 */
% base case - having a single stone means losing
win([1]) :- false.

