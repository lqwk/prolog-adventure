% this file contains all functionality related to the gameplay

:- dynamic rock/2, gem/2, peril/3, has_peril/3.

% the goal of the game is to find the hidden gem once the hero
% finds the hidden gem, the game ends and the hero wins.

win() :-
    heropos(X, Y),
    gem(X, Y),
    pickup_gem(X, Y),
    write('\nFound gem! You win!\n\n'),
    halt.

win() :- true.

pickup_gem(X, Y) :- retract( gem(X, Y) ).

start() :- (static_heropos(C, R)  -> assert(heropos(C, R)); true), fail.
start() :- (static_gem(C, R)      -> assert(gem(C, R)); true), fail.
start() :- (static_rock(C, R)     -> assert(rock(C, R)); true), fail.
start() :- (static_peril(C, R, M) -> assert(peril(C, R, M)); true), fail.
start() :- tick().
