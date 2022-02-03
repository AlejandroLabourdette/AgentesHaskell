module Main where

import Board
import ListUtils
import Elements
import Print
import Operations
import Generate

main :: IO ()
main = do
    putStrLn (printBoard board01)
    putStrLn (printBoard board02)
    putStrLn (printBoard board03)
    -- putStrLn (printBoard board04)
    -- putStrLn (printBoard board05)
    -- putStrLn (printBoard board06)
    -- putStrLn (printBoard board07)
    -- putStrLn (printBoard board08)
    -- putStrLn (printBoard board09)
    -- putStrLn (printBoard board10)
    -- putStrLn (printBoard board11)
    -- putStrLn (printBoard board12)
    -- print (getAdyEmptySlots board06 [(1,2)])

board01 = initializeBoard 6 6 876318
board02 = generateCribs board01 5
board03 = generateKids board02 6 
-- board04 = Board.add board03 (Obstacle 1 4) 
-- board05 = Operations.moveRght board04 (Kid 1 1) 
-- board06 = Operations.moveRght board05 (Kid 1 2) 
-- board07 = Operations.moveRght board06 (Kid 1 3) 
-- board08 = Operations.moveRght board07 (Kid 1 4)
-- board09 = Board.add board08 (Robot 2 5 False)
-- board10 = Operations.moveUp board09 (Robot 2 5 False)
-- board11 = Operations.moveDwn board10 (Robot 1 5 True)
-- board12 = Operations.dropKid board11 (Robot 2 5 True)

