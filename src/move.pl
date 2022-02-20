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
        has_peril(X, Y),
        writef('\t[ok]\n'),
        % a move uses a single clock tick
        tick()
    ;
        write('\t[FAILED]\n')
    ).

% derive the surrounding positions of the hero
rpos(X, Y) :- PY #= Y-1, heropos(X, PY).
lpos(X, Y) :- PY #= Y+1, heropos(X, PY).
upos(X, Y) :- PX #= X-1, heropos(PX, Y).
dpos(X, Y) :- PX #= X+1, heropos(PX, Y).

% shorthand commands for making moves
rr() :- rpos(X, Y), write('Moving right... '), goto(X, Y).
ll() :- lpos(X, Y), write('Moving left... '), goto(X, Y).
uu() :- upos(X, Y), write('Moving up... '), goto(X, Y).
dd() :- dpos(X, Y), write('Moving down... '), goto(X, Y).
