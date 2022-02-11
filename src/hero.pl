% contains functionality relate to the hero character

:- dynamic heropos/2, hero_hp/1, hero_stamina/1.

% print hero status like HP and stamina
print_hero_status() :-
    hero_hp(HP),
    hero_stamina(ST),
    writef('Hero HP: \t %d\nHero Stamina: \t %d\n', [HP, ST]).

% helper function to print hero location
print_hero_position() :-
    heropos(X, Y),
    writef('Hero Position: \t (%d, %d)\n', [X, Y]).

% print all info about the hero
print_hero() :-
    print_hero_position(),
    print_hero_status().
