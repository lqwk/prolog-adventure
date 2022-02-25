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

% ======================== weather system =======================
