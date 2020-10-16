# Game «Tetris»
This project is a tetris game written on Haskell  
Developed by Innopolis University students during the _Programming in Haskell_ course

## How to run:
#### Windows:
First you need to download the compiler

```
stack setup
```
After that, you can build and run the project
```
stack build & stack exec Tetris-exe
```
After first build, you can run the project just by
```
stack exec Tetris-exe
```

#### Ubuntu
Try to use the Windows approach, but we didn't check if it works

#### MacOS
Try to use the Windows approach, but we didn't check if it works

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
##### both players:
space = pause  
space = unpause  
space = restart (if game is over)

### Game rules
1. Random tetriminoes falls in a 10x20 field, starting from one cell inside the field
1. Initial speed is 1 tetrimino/sec, but there is an acceleration (0.5%/automove)
1. In flight, you can:
    1. rotate tetrimino 90° left or right;
    1. move it horizontally or vertically;
    1. immediately drop.
1. Tetrimino falls until it hits another tetrimino, or the bottom of the gaming field
1. If any row filled, it disappears and everything that is higher than it drops one cell down.  
1. If new tetrimino can't be spawned, you lose
1. You can get points:
    1. 1 point for each survived automove;
    1. 10 points for immediate tetrimino dropping;
    1. 100 points for each eliminated row.

## Improvements ideas
##### Good features:
1. Hold
##### Super features:
1. Animations
1. Sound effects

## Developers team
Vyacheslav Vasilev (v.vasilev@innopolis.ru)  
Kamil Alimov (k.alimov@innopolis.ru)
