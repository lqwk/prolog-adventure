% create a map of size 10 x 10
mapsize(10, 10).

% set the hero's starting position
heropos(1, 1).

% rocks
rock(1, 8).
rock(2, 2).
rock(2, 4).
rock(2, 5).
rock(3, 4).
rock(3, 7).
rock(3, 9).
rock(4, 4).
rock(4, 7).
rock(4, 8).
rock(4, 9).
rock(5, 2).

% walls
wall(1, 2).

% gem position
gem(5, 1).

% set time to start at 0
time(0).

% setup the hero
hero_health(100).
hero_stamina(100).

% setup the perils
peril(2, 1, -5).