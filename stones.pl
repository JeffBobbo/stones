/**
 * coursework yo
 *
 * So we have a little game where there's N piles of stones, of any quantity.
 * Two players take it in turns to take a number of stones from a pile, 
 * or an equal number of stones from two piles. The player to pick up the last
 * stone is the loser
 */

/**
 * initial test case: 3 piles of 3, 2, and 1 stones respectively
 */

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
% removing from an empty list just gives an empty list
remove([], []).
% removes N from Head
remove([Head|Tail], [Head1|Tail1], N) :-
  Head >= N,
  Head1 = Head - N,
  remove(Tail, Tail1).

%remove().

move([_|O], O).
move([F|O],[NF|O]) :-
  between(1, F, X),
    NF is F - X,
    X < F.
move([F, O], [F, NO]) :-
  move(O, NO).

%remove_s([F|O],[F|NO],X) :-

/*
remove_n2([H|T], L1, X) :-
  proper_length(T, L),
  L > 0,
  L2 is T,
  remove_n(L2, L1, X),
  remove_n(L2).
*/

/*
move(S1, S2) :-
  proper_length(S1, L),
  move(S1, S2, L).
move(S1, S2, P) :-
  P > 0,
  nth1(P, S1, H),
  between(1, H, E),
  (
    remove(S1, S2, E),
    print(S2),
    nl
  );
  P1 is P-1,
  move(S1, S2, P1).
*/

move([F|O], [F, NO]) :-
  move(O, NO).
move(F, NO) :-
  between(0, F, X),
  (
    NF is F - X,
    NF > 0 -->
      Result = [NF|NO];
      Result = NO
  );
  fail.

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
% recursively test
win(S) :-
  move(S, S1),
  not(win(S1)).
