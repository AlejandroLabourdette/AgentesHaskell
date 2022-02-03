module PositionUtils where

import Board
import Elements


calcPosX :: Displayable a => Board -> a -> Int -> Int
calcPosX board element direction
    | posX < 1 || posX > lengthBoard board = x element
    | otherwise = posX
    where
        posX = x element + direction
calcPosY :: Displayable a => Board -> a -> Int -> Int
calcPosY board element direction
    | posY < 1 || posY > widthBoard board = y element
    | otherwise = posY
    where
        posY = y element + direction