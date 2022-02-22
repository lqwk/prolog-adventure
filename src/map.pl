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
% maps start from upper left corner (1,1) position and advance
% downwards and rightwards by increasing the (row, col) position
% -----------------------------------------------------------------


% calculate mapsize from nrows and ncols
mapsize(Rows, Cols) :- maprows(Rows), mapcols(Cols).


% 0:  empty:  walkable spot
process_cell(0, _, _) :- true.
% 1:  wall:   not walkable
process_cell(1, C, R) :- assert( wall(C, R) ).
% 2:  start:  hero starts adventure at this location
process_cell(2, C, R) :- assert( heropos(C, R) ).
% 3:  gem:    if found, game ends
process_cell(3, C, R) :- assert( gem(C, R) ).
% 4:  rock:   initially not walkable, but can be broken
process_cell(4, C, R) :- assert( rock(C, R) ).


% process cols
% base case: set number of cols, check whether ncols match
process_cols([], _, Ncol) :-
    Cols #= Ncol - 1,
    (
        mapcols(C)
    ->
        % ensure that ncols matches previously asserted value
        (C #= Cols -> true; write('Number of cols mismatch, aborting\n\n'), halt)
    ;
        assert( mapcols(Cols) )
    ).
% recursive case: extract current cell and process
process_cols([Head|Tail], Nrow, Ncol) :-
    % process current position (Ncol, Nrow)
    process_cell(Head, Ncol, Nrow),
    % process next col recursively
    NNcol #= Ncol + 1, process_cols(Tail, Nrow, NNcol).


% process rows
% base case: done processing, set number of rows
process_rows([], Nrow) :- Rows #= Nrow - 1, assert( maprows(Rows) ).
% recursive case: extract row and process, then advance to next row
process_rows([Row|Rem], Nrow) :-
    % process currently extracted row
    process_cols(Row, Nrow, 1),
    % advance to next row recursively
    NNrow #= Nrow + 1, process_rows(Rem, NNrow).


% helper function to setup map from a 2D grid
setup() :- map(M), process_rows(M, 1).
