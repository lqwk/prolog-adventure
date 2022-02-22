:- use_module(library(clpfd)).

% checks whether position is within bounds of (X, Y)
in_bounds(X, Y) :-
    mapsize(MX, MY),
    X #=< MX,
    Y #=< MY,
    X #> 0,
    Y #> 0.

% check if a move is valid
can_move(X, Y) :-
    not(rock(X, Y)),
    not(wall(X, Y)).

% move the hero to position (X, Y)
goto(X, Y) :-
    (
        % checks whether position is within bounds of (X, Y)
        in_bounds(X, Y),
        % check position (X, Y) is valid for a move
        can_move(X, Y)
    ->
        % get hero's current position
        heropos(CX, CY),
        % remove hero from current location
        retract( heropos(CX, CY) ),
        % move hero to new location
        assert( heropos(X, Y) ),
        % If there is a peril, fight it
        fight_peril(X, Y),
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
