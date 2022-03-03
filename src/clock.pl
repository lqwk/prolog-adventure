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
    assert( time(NT) ).

% defines all actions that happens during each clock tick
tick() :-
    % increment and print global time
    inc_time(), print_time(),
    % change the weather globally
    change_all_weather(),
    % check whether the game has ended
    win(),
    % print discovery information
    discover().

% helper to print time information
print_time() :-
    % print the day-night cycle time
    (is_day() -> writef('[🌞]: ') ; true),
    (is_night() -> writef('[🌙]: ') ; true),
    % print the current time
    time(T), writef('Time: %d\n', [T]).
