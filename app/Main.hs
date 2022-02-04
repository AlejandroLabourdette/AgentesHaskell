module Main where

import Board
import ListUtils
import Elements
import Print
import Operations
import Generate
import Objectives
import AgentsTurn

main :: IO ()
main = do
    -- putStrLn (printBoard board01)
    -- putStrLn (printBoard board02)
    -- putStrLn (printBoard board03)
    -- putStrLn (printBoard board04)
    putStrLn (printBoard board05)
    putStrLn (printBoard board06)
    putStrLn (printBoard board07)
    putStrLn (printBoard board08)
    putStrLn (printBoard board09)
    putStrLn (printBoard board10)
    putStrLn (printBoard board11)
    putStrLn (printBoard board12)
    -- print (computeObjectives board05)
    -- print (programCycle board05 10)

-- board01 = initializeBoard 6 6 876318
board01 = initializeBoard 6 6 876314
board02 = generateCribs board01 2
board03 = generateKids board02 3
board04 = generateRobots board03 3 
board05 = generateDirts board04 3 
board06 = doAgentsTurn board05 
board07 = doAgentsTurn board06  
board08 = doAgentsTurn board07
board09 = doAgentsTurn board08
board10 = doAgentsTurn board09  
board11 = doAgentsTurn board10
board12 = doAgentsTurn board11

-- board10 = Operations.moveUp board09 (Robot 2 5 False)
-- board11 = Operations.moveDwn board10 (Robot 1 5 True)
-- board12 = Operations.dropKid board11 (Robot 2 5 True)

programCycle :: Board -> Int -> String
programCycle board 0 = printBoard board ++ "FINITO"
programCycle board iter = 
    let
        newBoard = doAgentsTurn board
    in
        printBoard board ++ programCycle newBoard (iter-1)
    