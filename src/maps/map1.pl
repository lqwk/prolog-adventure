% create map
map([
    [2, 0, -5, 0, 1, 0, 0, 0, 0, 0],
    [1, 0, 0, 3, 1, 0, 1, 0, 0, 1],
    [0, 1, 0, 0, 1, 0, 1, 1, 0, 1],
    [0, 1, 0, 0, 1, 0, 1, 0, 0, 1],
    [0, 1, 0, 0, 1, 0, 1, 0, 1, 0],
    [0, 1, 0, 0, 1, 0, 1, 0, 1, 0],
    [0, 0, 1, 0, 1, 0, 1, 0, 0, 0],
    [0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 1, 0, 0, 0, 0, 1],
    [0, 0, 0, 0, 0, 0, 1, 0, 0, 1]
]).

% set time to start at 0
time(0).

% setup the hero
hero_health(100).
hero_stamina(100).

% set the percentage of cells that are affected by
% weather changes on each clock cycle tick
weather_influence(0.25).

monster_health(100).
hero_att(20).
