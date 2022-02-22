:- use_module(library(clpfd)).
:- dynamic peril/3.

fight_peril(X, Y) :-
    (
        peril(X, Y, M)
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
        print_hero_status()
    ;
        true
    ).

