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
##### 1 player:  
w = drop  
a = move left  
s = move down  
d = mode right  
x = rotate 90° left  
c = rotate 90° right  
##### 2 player:  
🠁 = drop  
🠀 = move left  
🠃 = move down  
🠂 = mode right  
k = rotate 90° left  
l = rotate 90° right 



### Game rules
Random tetriminoes falls from above into a rectangular field 10 cells wide and 20 cells high.  
In flight, the player can rotate the figure 90° left or right and move it horizontally or vertically.  
The tetrimino flies until it hits another tetrimino or the bottom of the gaming field.  
If any row filled, it disappears and everything that is higher than it drops one cell down.  
If the game can't produce new tetrimino, you lose, and the game starts from the beginning.  
You get 1 score point for each second of surival, 15 points for immediate tetrimino dropping, and 100 for each eliminated row

## Improvements ideas
##### Good features:
1. Hold
##### Super features:
1. Animations
1. Sound effects

## Developers team
Vyacheslav Vasilev (v.vasilev@innopolis.ru)  
Kamil Alimov (k.alimov@innopolis.ru)
