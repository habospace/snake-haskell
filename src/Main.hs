{-# Language NoMonomorphismRestriction #-}

import Ui
import Lib
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = play window background fps gs render' handleKeys update where
    window = InWindow "Snake" (displaySize+offset*2, displaySize+offset*2) (0, 0)
    render' = render snakeColor foodColor frameColor displaySize displaySize offset cellSide
    gs = initGame gameSize gameSize North
    background = black
    snakeColor = dark red
    foodColor = white
    frameColor = yellow
    cellSide = displaySize / gameSize
    displaySize = 400
    offset = 1
    gameSize = 50
    fps = 20
