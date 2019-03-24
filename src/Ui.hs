module Ui where

import Lib
import Graphics.Gloss.Interface.Pure.Game

render :: Color -> Color -> Color -> Float -> Float -> Float -> Float -> GameState -> Picture
render sc fc frc w h offset side (GameState _ _ (Snake b _ _) _ f _ _) = img where
    img = pictures $ [frame, foodPic] ++ bodyPics
    frame = color frc $ rectangleWire (w+offset*2) (h+offset*2)
    foodPic = translatePic w h side f $ makeCellPic fc side
    bodyPics = zipWith (translatePic w h side) b bodyPics'
    bodyPics' = (makeCellPic sc) <$> (replicate (length b) side)

    makeCellPic :: Color -> Float -> Picture
    makeCellPic c side = color c $ rectangleSolid side side

    translatePic :: Float -> Float -> Float -> Location -> Picture -> Picture
    translatePic width height side (x, y) p = translate dx dy p where
        x' = ((read $ (show x)) :: Float)
        y' = ((read $ (show y)) :: Float)
        dx = (- (width / 2) + (x' * side) - (side / 2)) + offset
        dy = (- (height / 2) + (y' * side) - (side / 2)) + offset

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (Char c) Down _ _) gs@(GameState w h _ _ _ _ _) = 
    case c of 
        'w' -> changeDirection North gs
        'a' -> changeDirection West gs
        's' -> changeDirection South gs
        'd' -> changeDirection East gs
        'r' -> initGame w h North
        _   -> gs
handleKeys _ gs = gs

update :: Float -> GameState -> GameState
update _ gs = tick gs 
