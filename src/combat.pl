:- use_module(library(clpfd)).
:- dynamic turn/2, fight/1, monster_health/1.

% checks whether position is within bounds of (X, Y)
hero_alive(H1) :-
    H1 #> 0.

% check if a move is valid
monster_alive(H2) :-
    H2 #> 0.

% move the hero to position (X, Y)
turn(C, R) :-
    (
        peril(C, R, M)
    ->
        write('\nFound a peril! Fighting!\n\n'),
        fight(C, R, M)
    ;
        true
    ).

% defines everything that happens during each turn
fight(C, R, M) :-
    (
        hero_health(H1),
        monster_health(H2),
        writef('Hero Health: \t\t %d\n', [H1]),
        writef('Monster Health: \t %d\n', [H2]),
        % checks whether hero is alive or die
        hero_alive(H1),
        % checks whether monster is alive or die
        monster_alive(H2)
    ->
        hero_attack(A),
        roll(D1),
        roll(D2),
        % impact health by margin
        HH #= H1 + D2 * M, % hero health
        MH #= H2 - D1 * A, % monster health
        writef('Hero Deals Damge: \t %d x %d\n', [D1, A]),
        writef('Monster Deals Damage: \t %d x %d\n\n', [D2, M]),
        % update health with NH
        retract( hero_health(H1) ),
        assert( hero_health(HH) ),
        retract( monster_health(H2) ),
        assert( monster_health(MH) ),
        % clock cycle
        tick_without_discover(),
        % continue fighting
        fight(C, R, M)
    ;
        write('\nBattle Result:\n'),
        % print current health
        print_hero_status(),
        % remove the peril from the map
        retract( peril(C, R, M) ),
        write('\t[BATTLE END]\n'),
        true
    ).
