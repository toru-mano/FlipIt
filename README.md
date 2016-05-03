# FlipIt

![overview](/doc/gif/overview.gif)

## Overview

FlipIt is a single player board game. Your goal is to make all tiles turn white. Implemented in [Haskell](https://www.haskell.org) and C. Inspired by the following web game.

- http://www.maxgames.com/play/flip-it.html

## Demo

Cheat mode shows an answer.

![cheat-mode](/doc/gif/cheat-mode.gif)

You can play larger board.

![large-mode](/doc/gif/large-mode.gif)

You can play more than two color. The following is a three color game.

![tricolor](/doc/gif/tricolor.gif)

## Install

``` sh
git clone https://github.com/toru-mano/FlipIt.git
cd FlipIt
stack build
```

## Usage

To start two color 5x5 size game.

```sh
stack exec FlipIt-exe
```

To start three color 6x6 size game.
```sh
stack exec -- FlipIt-exe 3 6
```

## Author

[Toru Mano](https://github.com/toru-manoa)
