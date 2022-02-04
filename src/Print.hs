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
_printElement board x y =
    _printKid board x y ++ _printRobot board x y ++ _printObstacle board x y ++_printDirt board x y++_printCrib board x y
    

_printKid :: Board -> Int -> Int -> String 
_printKid board x y 
    | exist (Kid x y) (kids board) = "k" -- falta cuando el bot suelta al ninho en la cuna
    | otherwise = " "
_printRobot :: Board -> Int -> Int -> String 
_printRobot board x y
    | exist (Robot x y True) (robots board) = "R"
    | exist (Robot x y False) (robots board) = "r"
    | otherwise = " "
_printObstacle :: Board -> Int -> Int -> String 
_printObstacle board x y
    | exist (Obstacle x y) (obstacles board) = "o"
    | otherwise = " "
_printDirt :: Board -> Int -> Int -> String 
_printDirt board x y
    | exist (Dirt x y) (dirts board) = "d"
    | otherwise = " "
_printCrib :: Board -> Int -> Int -> String 
_printCrib board x y
    | exist (Crib x y True) (cribs board) = "C"
    | exist (Crib x y False) (cribs board) = "c"
    | otherwise = " "