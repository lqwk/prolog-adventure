% contains facts and rules for systems within the world

:- use_module(library(clpfd)).


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

% ======================== weather system =======================
