% this file contains logic related to fighting perils

:- use_module(library(clpfd)).
:- dynamic peril/3.

fight_peril(C, R) :-
    (
        peril(C, R, M)
    ->
        % get current health
        hero_health(H),
        write(' Found a peril! Fighting!\n After fighting with peril, current status is: \n'),
        % impact health by margin
        NH #= H + M,
        % update health with NH
        retract( hero_health(H) ),
        assert( hero_health(NH) ),
        % print current health
        print_hero_status(),
        % remove the peril from the map
        retract( peril(C, R, M) )
    ;
        true
    ).
