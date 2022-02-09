:- use_module(library(clpfd)).

% move from (CX, CY) to (X, Y)
move(CX, CY, X, Y) :-
    % remove hero from current location
    retract( heropos(CX, CY) ),
    % move hero to new location
    assert( heropos(X, Y) ),
    writef('Moved from (%d, %d) to (%d, %d)', [CX, CY, X, Y]).

% move right by 1 step
move_r() :- heropos(X, Y), NX #= X+1, move(X, Y, NX, Y).
% move left by 1 step
move_l() :- heropos(X, Y), NX #= X-1, move(X, Y, NX, Y).
% move up by 1 step
move_u() :- heropos(X, Y), NY #= Y+1, move(X, Y, X, NY).
% move down by 1 step
move_d() :- heropos(X, Y), NY #= Y-1, move(X, Y, X, NY).
