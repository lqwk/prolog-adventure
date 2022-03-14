# Install Dependencies

Download SWI-Prolog

On MacOS, you can use Homebrew to install by running the following:

```bash
brew install swi-prolog
```

# Starting the game

Run the following to start the game

```bash
make run
```

Then run the following to start the game

```prolog
start().
```

# Playing the game

To move around, run the following commands

* `rr().`: Move right
* `ll().`: Move left
* `uu().`: Move up
* `dd().`: Move down
* `stay()`: Stay put at current location

# Goal

The goal of the game is to find the hidden gem. When the gem is found, the game ends and you win.
