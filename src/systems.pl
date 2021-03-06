% contains facts and rules for systems within the world

:- use_module(library(clpfd)).
:- dynamic weather/3.


% ======================= day-night cycle =======================

% the day-night cycle is derived from ticks in the global clock
% daytime is defined to be within ticks [0, 12)
% nighttime is defined to be within ticks [12, 24)

is_day() :- time(T), T mod 24 #< 12.
is_night() :- time(T), T mod 24 #>= 12, T mod 24 #< 24.

% ======================= day-night cycle =======================




% ======================= weather system ========================

% The main purpose of the weather system is to introduce some
% uncertainty into the world map.
%
% Weather is defined for each cell in the grid and may change
% randomly. The weather for each cell is generated and defined by
% a random number. Not all cells may have weather associated with
% it. For example, during a certain clock cycle on a 10 x 10 map,
% we might just change the weather for 50 cells out of 100 cells.


% bind the number of total cells N to C * R
total_cells(N) :- mapsize(C, R), N #= C * R.

% number of cells affected by weather changes on each tick
weather_affected_cells(N) :-
    total_cells(T), TT is float(T),
    weather_influence(P), PP is float(P),
    NN is TT * PP,
    N is ceiling(NN).


% listed below are the different kinds of weather conditions:
%
%   1. clear:    nothing happens in clear weathers!
%   2. rainy:    -1 stamina on each clock cycle
%   3. thunder:  -1 stamina and -1 health on each clock cycle
%   4. foggy:    cannot see stuff on the cell


% randomly generate a weather condition
random_weather(W) :- random_between(1, 4, W).

% change the weather at (C, R) to W
set_weather(_, _, W) :- W #= 1, fail.
set_weather(C, R, W) :- W #= 2, assert( weather(C, R, rainy) ), fail.
set_weather(C, R, W) :- W #= 3, assert( weather(C, R, thunder) ), fail.
set_weather(C, R, W) :- W #= 4, assert( weather(C, R, foggy) ), fail.
set_weather(_, _, _).

% clear previous weather conditions
clear_weather(C, R) :- weather(C, R, W), retract(weather(C, R, W)), fail.
clear_weather(_, _).

% randomly change the weather of cell (C, R)
change_weather(C, R) :-
    % randomly generate weather condition
    random_weather(W),
    % retract previous weather condition
    clear_weather(C, R),
    % set the new weather
    set_weather(C, R, W),
    % fallthrough
    fail.

change_weather(_, _).


% randomly generate a cell on the map
random_cell(C, R) :-
    % get the size of the map
    mapsize(MC, MR),
    % randomly generate (C, R) pairs to change weather for
    random_between(1, MC, C), random_between(1, MR, R).

% recursive definition for changing the weather for n cells
change_weather_for_n_cells(N) :-
    % boundary condition
    N #> 0,
    % randomly get a cell on the map
    random_cell(C, R),
    % actually change the weather
    change_weather(C, R),
    % loop condition
    S #= N-1, change_weather_for_n_cells(S), fail.

change_weather_for_n_cells(_).

% randomly choose N cells to change the weather
% N is given by weather_affected_cells(N)
change_all_weather() :-
    % get the number of cells to change weather for
    weather_affected_cells(N),
    % change the weather for a total of N cells
    change_weather_for_n_cells(N).


% deal damage to hero's health/stamina accordingly
weather_damage() :-
    % hero's position and weather
    heropos(C, R), weather(C, R, rainy),
    % deal damage to hero's stamina
    hero_stamina(S), NS #= S-1,
    retract( hero_stamina(S) ),
    assert( hero_stamina(NS) ),
    fail.

weather_damage() :-
    % hero's position and weather
    heropos(C, R), weather(C, R, thunder),
    % deal damage to hero's stamina
    hero_stamina(S), NS #= S-1,
    retract( hero_stamina(S) ),
    assert( hero_stamina(NS) ),
    % deal damage to hero's health
    hero_health(H), NH #= H-1,
    retract( hero_health(H) ),
    assert( hero_health(NH) ),
    fail.

weather_damage().

% ======================== weather system =======================
