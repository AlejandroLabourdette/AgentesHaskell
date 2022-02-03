module Print (printBoard) where

import Board
import Elements
import ListUtils

printBoard :: Board -> String
printBoard board = _printBoard n m board 
    where
        n = lengthBoard board
        m = widthBoard board

_printBoard :: Int -> Int -> Board -> String
_printBoard 0 m b = ""
_printBoard n 0 board = "\n" ++ _printBoard (n-1) width board 
    where 
        width = widthBoard board
_printBoard n m board = start ++ _printElement board x y ++ "|" ++ _printBoard n (m-1) board
    where
        x = lengthBoard board - (n-1)
        y = widthBoard board - (m-1)
        start = if y == 1 then "|" else "" 

_printElement :: Board -> Int -> Int -> String 
_printElement board x y
    | elementType == kidType = _printKid board x y
    | elementType == robotType = _printRobot board x y
    | elementType == obstacleType = _printObstacle board x y
    | elementType == dirtType = _printDirt board x y
    | elementType == cribType = _printCrib board x y
    | otherwise = _printSpace
    where
        elementType = get board x y

_printKid :: Board -> Int -> Int -> String 
_printKid board x y 
    | exist (Robot x y True) (robots board) = "L " -- falta cuando el bot suelta al ninho en la cuna
    | exist (Robot x y False) (robots board) = "RK"
    | otherwise = "K "
_printRobot :: Board -> Int -> Int -> String 
_printRobot board x y
    | exist (Dirt x y) (dirts board) = "RD"
    | exist (Crib x y False) (cribs board) = "RC"
    | otherwise = "R "
_printObstacle :: Board -> Int -> Int -> String 
_printObstacle _ _ _ = "# "
_printDirt :: Board -> Int -> Int -> String 
_printDirt _ _ _ = "- "
_printCrib :: Board -> Int -> Int -> String 
_printCrib _ _ _ = "U "
_printSpace :: [Char]
_printSpace = "  "