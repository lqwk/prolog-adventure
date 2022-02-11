% contains functionality relate to the hero character

:- dynamic heropos/2.

% helper function to print hero location
print_hero_position() :-
    heropos(X, Y),
    writef('Hero Position: (%d, %d)\n', [X, Y]).
