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
    % checks whether position is within bounds of (X, Y)
    in_bounds(X, Y),
    % check position (X, Y) is valid for a move
    can_move(X, Y),
    % get hero's current position
    heropos(CX, CY),
    % remove hero from current location
    retract( heropos(CX, CY) ),
    % move hero to new location
    assert( heropos(X, Y) ),
    writef('Hero moved from (%d, %d) to (%d, %d)', [CX, CY, X, Y]).

goto(X, Y) :- writef('FAILED to move to (%d, %d)', [X, Y]).


% move right by 1 step
rr() :- heropos(X, Y), NX #= X+1, goto(NX, Y).
% move left by 1 step
ll() :- heropos(X, Y), NX #= X-1, goto(NX, Y).
% move up by 1 step
uu() :- heropos(X, Y), NY #= Y+1, goto(X, NY).
% move down by 1 step
dd() :- heropos(X, Y), NY #= Y-1, goto(X, NY).
