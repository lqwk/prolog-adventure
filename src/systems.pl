% contains facts and rules for systems within the world

:- use_module(library(clpfd)).

% ==================== day-night cycle ====================

% the day-night cycle is derived from ticks in the global clock
% daytime is defined to be within ticks [0, 12)
% nighttime is defined to be within ticks [12, 24)

is_day() :- time(T), T mod 24 #< 12.
is_night() :- time(T), T mod 24 #>= 12, T mod 24 #< 24.

% ==================== day-night cycle ====================
