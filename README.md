# Игра «Tetris»
It's a tetris game written on Haskell 

authors: Vyacheslav Vasilev & Kamil Alimov

## How to run:
First you need to download the compiler

```
stack setup
```
After that, you can build the project

Type `stack build & stack exec Tetris-exe` in cmd


### Rules of the game



Random tetrimino figures fall from above into a rectangular glass 10 cells wide and 20 cells high. In flight, the player can rotate the figure 90 ° and move it horizontally. You can also “drop” a figure, that is, accelerate its fall, when it has already been decided where the figure should fall. The figurine flies until it hits another figurine or the bottom of a glass. If at the same time a horizontal row of 10 cells is filled, it disappears and everything that is higher than it drops one cell.



#### Ubuntu

```
sudo apt-get update
sudo apt-get install -y cabal-install

cabal update
cabal install gloss
```
