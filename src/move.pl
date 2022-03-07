:- use_module(library(clpfd)).

% checks whether position is within bounds of (X, Y)
in_bounds(C, R) :-
    mapsize(MC, MR),
    C #=< MC,
    R #=< MR,
    C #> 0,
    R #> 0.

% check if a move is valid
can_move(C, R) :-
    % checks whether position is within bounds
    in_bounds(C, R),
    % check if position is valid for a move
    not(rock(C, R)),
    not(wall(C, R)).

% move the hero to position (X, Y)
goto(X, Y) :-
    (
        % check if we can move to (X, Y)
        can_move(X, Y)
    ->
        % get hero's current position
        heropos(CX, CY),
        % remove hero from current location
        retract( heropos(CX, CY) ),
        % move hero to new location
        assert( heropos(X, Y) ),
        % If there is a peril, fight it
        turn(X, Y),
        writef('\t[ok]\n'),
        % a move uses a single clock tick
        tick()
    ;
        write('\t[FAILED]\n'),
        false
    ).

% derive the surrounding positions of the hero
rpos(C, R) :- NC #= C-1, heropos(NC, R).
lpos(C, R) :- NC #= C+1, heropos(NC, R).
upos(C, R) :- NR #= R+1, heropos(C, NR).
dpos(C, R) :- NR #= R-1, heropos(C, NR).

% shorthand commands for making moves
rr() :- rpos(C, R), write('Moving right... '), goto(C, R).
ll() :- lpos(C, R), write('Moving left... '),  goto(C, R).
uu() :- upos(C, R), write('Moving up... '),    goto(C, R).
dd() :- dpos(C, R), write('Moving down... '),  goto(C, R).

% stay put at current location
stay() :- write('Staying put... \t[ok]\n'), tick().
