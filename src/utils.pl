% helper to print time information
print_time() :-
    % print the day-night cycle time
    (is_day() -> writef('[🌞]: ') ; true),
    (is_night() -> writef('[🌙]: ') ; true),
    % print the current time
    time(T), writef('Time: %d\n', [T]).


% helper to print the current status of the game
ps() :-
    % print time information
    print_time(),

    % print the hero's location
    heropos(X, Y), writef('Hero Position: (%d, %d)\n', [X, Y]),

    write('\n').
