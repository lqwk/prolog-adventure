% contains logic related to discovering cells around the hero

% show the information for the cell on which the hero is on
% cannot see cell info in foggy weather
show_cell_info(C, R) :- weather(C, R, foggy), write('Foggy, cannot see'), !.

% otherwise, print cell information
show_cell_info(C, R) :-
    show_object_info(C, R),
    write(' \t '),
    show_weather_info(C, R).

% show cell object information
show_object_info(C, R) :- wall(C, R), write('Wall'), fail.
show_object_info(C, R) :- rock(C, R), write('Rock'), fail.
show_object_info(C, R) :- gem(C, R), write('Gem'), fail.
show_object_info(C, R) :- peril(C, R, _), write('Peril'), fail.
show_object_info(_, _).

% show weather information
show_weather_info(C, R) :- weather(C, R, rainy), write('Rainy'), fail.
show_weather_info(C, R) :- weather(C, R, thunder), write('Thunder'), fail.
show_weather_info(_, _).

% the discovery module prints out information related to the 4
% adjacent cells to the left, right, up, down of the hero
discover() :-
    write('\n--------------------------\n'),
    write('Surroundings:\n\n'),
    % show info for current cell the hero is on
    heropos(C, R), write('\tHERO:\t'), show_cell_info(C, R), write('\n'),
    % show info for adjacent cells
    rpos(RC, RR), write('\trr():\t'), show_cell_info(RC, RR), write('\n'),
    lpos(LC, LR), write('\tll():\t'), show_cell_info(LC, LR), write('\n'),
    upos(UC, UR), write('\tuu():\t'), show_cell_info(UC, UR), write('\n'),
    dpos(DC, DR), write('\tdd():\t'), show_cell_info(DC, DR), write('\n'),
    write('--------------------------\n\n').
