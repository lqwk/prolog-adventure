% functionality related to creating game maps

:- use_module(library(clpfd)).
:- dynamic map/1.
:- dynamic mapsize/2, mapcols/1, maprows/1, heropos/2.
:- dynamic wall/2, gem/2, rock/2.


% -----------------------------------------------------------------
% maps are represented by a grid of numbers, keys are listed below
% note that his is only temporary, new keys can be added
%
%   0:  empty:  walkable spot
%   1:  wall:   not walkable
%   2:  start:  hero starts adventure at this location
%   3:  gem:    if found, game ends
%   4:  rock:   initially not walkable, but can be broken
%
%   -M: peril:  a negative number indicates a peril (monster)
%
% maps start from upper left corner (1,1) position and advance
% downwards and rightwards by increasing the (row, col) position
% -----------------------------------------------------------------


% calculate mapsize from nrows and ncols
length_of(N, L) :- length(L, N).
size(M, R, C) :- length(M, R), maplist(length_of(C), M).
mapsize(C, R) :- map(M), size(M, R, C).


% finds and binds position (C, R) to value V in map
find(C, R, V) :-
    % extract the map
    map(Map),
    % iterate through each row
    nth1(R, Map, Row),
    % iterate through each col
    nth1(C, Row, V).


% rules for deriving object positions, note that these
% positions are static and cannot be changed
wall(C, R)            :- find(C, R, 1).
static_heropos(C, R)  :- find(C, R, 2).
static_gem(C, R)      :- find(C, R, 3).
static_rock(C, R)     :- find(C, R, 4).
static_peril(C, R, M) :- find(C, R, M), M #< 0.
