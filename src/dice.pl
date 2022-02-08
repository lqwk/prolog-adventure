% Functionality related to dice

% Roll a dice - returns a random number in range [1,6]
roll(X) :- random_between(1, 6, X).
