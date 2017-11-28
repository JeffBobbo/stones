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
  not(slowin(S1)).

:- dynamic iswin/2.
reset :-
  retractall(iswin(_, _)).

% base cases
% having no stones means you've won
fastwin([]).
% having a single stone means losing
fastwin([1]) :- false,!.
% recursively test again, but this time, assert iswin if it's a win
% and don't back track
fastwin(S) :-
  msort(S, Ss),
  forall(move(Ss, S1),
  (
    not(iswin(S, S1)) ->
    (
      not(fastwin(S1)),
      assert(iswin(S, S1))
    )
  )).
fastwin_int(S, S1) :-
  not(iswin(S, S1)) ->
  (
    not(fastwin(S1)),
    assert(iswin(S, S1))
  ).
win(S) :-
  fastwin(S).


/**
 * Question 3
 * A predicate, analyse(S), that determines if it's a win for the player if they're playing on state S.
 */
analyse_move(S) :-
  write(S),
  nl,
  fail;
  true.

analyse(S) :-
  % call win, this'll populate iswin
  msort(S, Ss),
  win(Ss), !,
  (
    forall(iswin(Ss, S1), %(write(S1), nl))
    analyse_move(S1))
  );
  (
    print('There are no winning moves'),
    nl,
    fail
  ).

/**
 * Question 4
 * A predicate, analyseall(N), which calls analyse on every state of one or
 * two heaps of up to size N.
 * e.g., N = 3, [1], [2], [3], [1, 1], [1, 2], [1, 3], [2, 2], [2, 3], [3, 3]
 */
analyseall_internal(S) :-
  write(S),
  tab(4),
  analyse(S),
  nl.

analyseall(N) :-
  between(1, N, X),
  (
    between(X, N, Y),
    (
      S = [X|Y],
      analyseall_internal(S)
    ),
    analyseall_internal([X])
  ).
