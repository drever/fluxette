# fluxette

An implementation of [Set](https://en.wikipedia.org/wiki/Set_(game)), a game where a triple of cards have to be found. It is implemented using [react-flux](https://hackage.haskell.org/package/react-flux), a Haskell binding to react flux. It can be tested online at the [github project page](https://drever.github.io/fluxette/). (Currently it uses always the same random seed for debugging purposes).

# Getting started

The setup works analogous to the setup described in [react-flux](https://bitbucket.org/wuzzeb/react-flux). 

# Backend

There is also a backend with servant which serves the game state with a servant server (https://github.com/drever/fluxetteback). The idea is that there is a single game session and multiple players can play in competition modes. It is half finished and the development is currently on hold [TODO.md](https://github.com/drever/fluxetteback/blob/master/TODO.md).
