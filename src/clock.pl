% handles the clocking behavior of our clock-based world

:- use_module(library(clpfd)).
:- dynamic time/1.

% increment global time count by 1 tick
inc_time() :-
    % get current time
    time(T),
    % increment time by 1 tick
    NT #= T + 1,
    % update time with NT
    retract( time(T) ),
    assert( time(NT) ),
    % print time information
    print_time().

% defines all actions that happens during each clock tick
tick() :-
    % increment global time
    inc_time(),
    % check whethe the game has ended
    win().
