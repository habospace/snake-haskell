# snakeHaskell

This is my implementation of the popular snake game written in Haskell.
The source code of the game is divided into the following 3 modules:

```
 |src/
   L Lib.hs (1.)
   L UI.hs (2.)
   L Main.hs (3.)
```

**(1.)** Lib.hs contains the logic of the game written in pure Haskell.
The **tick** function defines the update of the GameState from iteration
**n** to **n + 1**.

**(2.)** UI.hs contains the UI rendering logic of the game. It's using the 
**Gloss** library of Haskell (Graphics.Gloss).

**(3.)** Main.hs is the only impure part (involving IO) of the Game. It's
the main file that instantiates the Game.

