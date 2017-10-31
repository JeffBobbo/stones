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

take([P|S], N, St) :-
  P1 is P-N,
  P1 > 0,
  St = [P1|S],
  take(St, N, []).