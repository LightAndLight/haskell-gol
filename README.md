# haskell-gol
Multithreaded implementation of Conway's Game of Life

I wrote this one evening because I hadn't ever written a multithreaded application in Haskell.
It's based on COMP3301's first assignment for 2016.

## Usage

`haskell-gol-exe <WIDTH> <HEIGHT> <REFRESH_RATE (ms)>`

An interactive shell will appear. Valid commands:

- still (block|loaf|boat|beehive) X Y
- osc (toad|blinker|beacon) X Y
- ship glider X Y
- cell (alive|dead) X Y
- start
- stop
- clear
