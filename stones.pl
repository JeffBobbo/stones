/*
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
 *
 * Some credit should be given to Ashley Scopes for Q1, comments on it and the
 * rest of the assignment is my own work.
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
% removes the head, aka taking the entire pile
remove_h([_|Tail], Tail).

% make a move by removing the first pile
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
 * Question 2
 * Return true if S is a winning position
 */
:- dynamic iswin/2.
% resets the dynamic predicate, for ease of testing
reset :-
  retractall(iswin(_, _)).

% base case, having 0 at the start of your turn means you've won
test([]).
% base case, having 1 at the start of your turn means you've lost
test([1]) :- fail,!.
% test if this is a win, using either the dynamic iswin predicate or
% recursively testing.
test(S) :-
  msort(S, Ss), % sort them
  move(Ss, Sn), % make a move
  msort(Sn, Sns), % sort the move
  (
    iswin(Ss, Sns) ->
      % if the move has already been worked out, fail, otherwise, do stuff
      true
    ;
      not(test(Sns)),
      assert(iswin(Ss, Sns))
  ).
% failure driven loop to build the entire tree of winning possibilities
% from a given state, S.
build(S) :-
  test(S),
  fail.
build(_) :-
 true.

% sort the given state, build the win-tree and just return true if there's
% any winning move to make
win(S) :-
  msort(S, Ss),
  build(Ss),
  iswin(Ss, _),!.

/*
 * Question 3
 * A predicate, analyse(S), that determines if it's a win for the player
 * and which moves they can play
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

/*
 * Question 4
 * A predicate, analyseall(N), which calls analyse on every state of one or
 * two heaps of up to size N.
 * e.g., N = 3, [1], [2], [3], [1, 1], [1, 2], [1, 3], [2, 2], [2, 3], [3, 3]
 */
% writes out one line, e.g.,
% S = [2, 3], L = [[1], [2, 2]] becomes
% [2,3]:  [[1],[2,2]]
analyseall_internal(S, L) :-
  write(S),
  write(':\t'),
  (
    L \= [] ->
      write(L)
    ;
      write('no winning moves.')
  ),
  nl.

analyseall(N) :-
  between(1, N, X), % for the first pile
  (
    findall(Sn, iswin([X], Sn), L), % find all winning moves
    analyseall_internal([X], L); % write them all
    between(X, N, Y), % the second pile, start from X so no repeats
    (
      S = [X,Y],
      findall(Sn, iswin(S, Sn), L), % same as above
      analyseall_internal(S, L)
    )
  ),fail,!. % force backtracking, so no interaction required


/*
 * Question 5
 * A play predicate, play, to play a game. One player is the user, the other is the computer
 */
% queries the player for input, player gives their turn by specifying
% what the end state is. It's checked against iswin, which if fails will
% redo on the second predicate, which just recursively calls back again to
% reprompt the user for correct input.
player_input(States, Sout) :-
  read(Sout),
  member(Sout, States).

player_turn(Sin, Sout) :-
  write('Current state: '),
  write(Sin),
  write('\nPossible moves: '),
  findall(Ms, (move(Sin, M), msort(M, Ms)), Movs),
  sort(Movs, Moves), % sort moves so that we get consistent output
  write(Moves), % also take advantage that sort removes duplicates
  write('\nEnter state after move (or use "^C . RETURN a" to quit):\n'),
  catch(player_input(Moves, Sout), _, fail),
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
  build(S),
  do_play(S, 1).
