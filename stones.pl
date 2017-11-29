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
/*
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
*/
fastwin([]).
fastwin([1]) :- fail,!.
fastwin(S) :-
  msort(S, Ss),
  (
    iswin(Ss, _) ->
    (
      true
    )
    ;
    (
      move(Ss, S1),
%      write('Evaluating: '),
%      write(Ss),
%      write(' -> '),
%      write(S1),
%      nl,
      not(fastwin(S1)),
      assert(iswin(Ss, S1))
%       findall(Sn, move(Ss, Sn), Moves),
%       fastwin_int(Ss, Moves)
    )
  ).

fastwin_int(_, []) :- fail ; true.
fastwin_int(S, [Sn|States]) :-
  not(fastwin(Sn)),
  assert(iswin(S, Sn)),
  fastwin_int(S, States).

buildwin(S) :-
  msort(S, Ss),
  (
    iswin(Ss, _) ->
    (
      true
    )
    ;
    (
      move(Ss, S1),
      not(win(S1)),
      assert(iswin(Ss, S1))
    )
  ),
  fail.

win([]).
win([1]) :- fail,!.
win(S) :-
  %buildwin(S);
  %iswin(S, _).
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
      S = [X,Y],
      analyseall_internal(S)
    ),
    analyseall_internal([X])
  ).


/**
 * Question 5
 * A play predicate, play, to play a game. One player is the user, the other is the computer
 */
% queries the player for input, player gives their turn by specifying
% what the end state is. It's checked against iswin, which if fails will
% redo on the second predicate, which just recursively calls back again to
% reprompt the user for correct input.
player_turn(Sin, Sout) :-
  write('Current state: '),
  write(Sin),
  write('\nPossible moves: '),
  findall(M, move(Sin, M), Moves),
  write(Moves),
  write('\nEnter state after move\n> '),
  read(Sout),
  move(Sin,Sout), !.
player_turn(Sin, Sout) :-
  write('Invalid play, please try again.\n'),
  player_turn(Sin, Sout).

% ust play the first thing in iswin, don't backtrack
computer_turn(Sin, Sout) :-
  iswin(Sin, Sout),!.
% or the first move if there's no win, don't backtrack
computer_turn(Sin, Sout) :-
  move(Sin, Sout),!.

game_over(S, P) :-
  S = [],
  P = 1,
  write('The foolish human has been rendered a loser!\n').
game_over(S, _) :-
  S = [],
  write('The mere human has beaten me, forsooth!\n').

do_play(S, P) :-
  P = 1,
  player_turn(S, Sout),
  (
    game_over(Sout, 1) ->
      fail
    ;
      do_play(Sout, 0)
  ).
do_play(S, P) :-
  P = 0,
  computer_turn(S, Sout),
  (
    game_over(Sout, 0) ->
      fail
    ;
      do_play(Sout, 1)
  ).

play(S) :-
  win(S),!,
  do_play(S, 1).
play(S) :-
  do_play(S, 1).
