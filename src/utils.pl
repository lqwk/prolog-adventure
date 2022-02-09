% helper to print the current status of the game
status() :-
    % print the hero's location
    heropos(X, Y), writef('Hero Position: (%d, %d)', [X, Y]).
