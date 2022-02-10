% this file contains all functionality related to the gameplay

:- dynamic heropos/2, rock/2, gem/2, active/1.

% the goal of the game is to find the hidden gem
% once the hero finds the hidden gem, the game ends
% and the hero wins.

win() :-
    active(game),
    heropos(X, Y),
    gem(X, Y),
    pickup_gem(X, Y),
    retract( active(game) ),
    write('Found gem! You win!\n\n'),
    halt.

pickup_gem(X, Y) :- retract( gem(X, Y) ).
