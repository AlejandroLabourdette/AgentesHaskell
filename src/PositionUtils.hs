module PositionUtils where

import Board
import Elements

infinite :: Int
infinite = 99999

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

calcPos :: Displayable a => Board -> a -> Int -> Int -> Ghost
calcPos board slot incX incY =
    let 
        newX = calcPosX board slot incX
        newY = calcPosY board slot incY
    in
        Ghost newX newY

getDirectionType :: (Displayable a, Displayable b) => a -> b -> [Char]
getDirectionType start end
    | difX == -1 && difY == 0 = upDirType
    | difX == -2 && difY == 0 = up2DirType
    | difX == 1 && difY == 0 = dwnDirType
    | difX == 2 && difY == 0 = dwn2DirType
    | difX == 0 && difY == 1 = rghtDirType
    | difX == 0 && difY == 2 = rght2DirType
    | difX == 0 && difY == -1 = lftDirType
    | difX == 0 && difY == -2 = lft2DirType    
    | otherwise = error "No direction lead to that slot"
    where
        difX = x end - x start
        difY = y end - y start 

upDirType = "upDir"
up2DirType = "up2Dir"
dwnDirType = "dwnDir"
dwn2DirType = "dwn2Dir"
lftDirType = "lftDir"
lft2DirType = "left2Dir"
rghtDirType = "rghtDir"
rght2DirType = "rght2Dir"

