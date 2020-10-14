# Game «Tetris»
This project is a tetris game written on Haskell  
Developed by Innopolis University students during the _Programming in Haskell_ course

## How to run:
#### Windows:
First you need to download the compiler

```
stack setup
```
After that, you can build the project by typing  
```
stack build & stack exec Tetris-exe
```
in cmd

#### Ubuntu

```
sudo apt-get update
sudo apt-get install -y cabal-install

cabal update
cabal install gloss
```

## Gameplay:
![gif](https://s1.gifyu.com/images/tetris-demo.gif)

### Control settings:
By keyboard:  
Arrows -> move tetrimino  
a -> rotate tetrimino by 90° left  
d -> rotate tetrimino by 90° right  

### Game rules
Random tetriminoes falls from above into a rectangular field 10 cells wide and 20 cells high.  
In flight, the player can rotate the figure 90° left or right and move it horizontally or vertically.  
The tetrimino flies until it hits another tetrimino or the bottom of the gaming field.  
If any row filled, it disappears and everything that is higher than it drops one cell down.  
If the game can't produce new tetrimino, you lose, and the game starts from the beginning.

## Improvements ideas
##### Must have:
1. Good game speed implementation
1. Button to drop immediately
1. Start, stop, restart, "You died"
1. Score += 100 for each eliminated row
1. Good rotation implementation:
    1. if cannot rotate –> move to the opposite side and then rotate
    1. rewrite that ugly shit
1. Improve design
##### Super features:
1. Two players competitive tetris
1. Animations
1. Sound effects

## Developers team
Vyacheslav Vasilev (v.vasilev@innopolis.ru)  
Kamil Alimov (k.alimov@innopolis.ru)
