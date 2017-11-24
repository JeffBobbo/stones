/**
 * coursework yo
 *
 * So we have a little game where there's N piles of stones, of any quantity.
 * Two players take it in turns to take a number of stones from a pile, 
 * or an equal number of stones from two piles. The player to pick up the last
 * stone is the loser
 */

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
% remove first element from the list, unifying with N
remove([N|Tail], Tail, N).
% removes N from Head, Head==N, do the one above
remove([Head|Tail], [NewHead|Tail], N) :-
  Head > N,
  NewHead is Head - N.
% removes N from each pile that can have N removed
remove([Head|Tail], [Head|NewTail], N) :-
  remove(Tail, NewTail, N).

% removes 1..N from Head
remove_h([Head|Tail], [NewHead|Tail]) :-
  N is Head - 1,
  between(1, N, I),
  (
    NewHead is Head - I;
    fail
  ).

% remove _, because we don't care about it
remove_h([_|Tail], Tail).

move([Head|Tail], NewHead) :-
  remove_h([Head|Tail], NewHead).

move([Head|Tail], Play) :-
  between(1, Head, I),
  (
    NewHead is Head - I,
    remove(Tail, NewTail, I),
    (
      NewHead > 0 ->
        Play = [NewHead | NewTail]
      ;
        Play = NewTail
    )
    ;
    fail
  ).

move([Head|Tail], [Head|NewTail]) :-
  move(Tail, NewTail).


/*
move([Head|Tail], Play) :-
  between()
move([Head|Tail],[NF|Tail]) :-
  between(1, Head, X),
    NF is Head - X,
    X < Head.
move([Head, Tail], [Head, NO]) :-
  move(Tail, NO).
*/

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

/*
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
*/

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
% this is really slow, because it reevaluates
% base case - having a single stone means losing
slowin([1]) :- false.
% recursively test
slowin(S) :-
  move(S, S1),
  not(win(S1)).

/**
 * Question 3
 * A predicate, analyse(S), that determines if it's a win for the player if they're playing on state S.
 */
analyse(S) :-
  slowin(S).