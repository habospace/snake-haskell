# snakeHaskell

This is my implementation of the popular snake game written in Haskell.
The source code of the game is divided into the following 3 modules:

```
 |src/
   ├ Lib.hs (1.)
   ├ UI.hs (2.)
   └ Main.hs (3.)
```

**(1.)** [src/Lib.hs](https://github.com/habospace/snakeHaskell/blob/master/src/Lib.hs) contains the logic of the game written in pure Haskell.
The **tick** function defines the update of the GameState from iteration
**n** to **n + 1**.

**(2.)** [src/UI.hs](https://github.com/habospace/snakeHaskell/blob/master/src/Ui.hs) contains the UI rendering logic of the game. It's using the 
**Gloss** library of Haskell (Graphics.Gloss).

**(3.)** [src/Main.hs](https://github.com/habospace/snakeHaskell/blob/master/src/Main.hs) is the only impure part (involving IO) of the Game. It's
the main file that instantiates the Game.

