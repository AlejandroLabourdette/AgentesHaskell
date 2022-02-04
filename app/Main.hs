module Main where

import Board
import ListUtils
import Elements
import Print
import Operations
import Generate
import Objectives
import AgentsTurn
import EnviromentTurn

main :: IO ()
main = do
    -- putStrLn (printBoard board01)
    -- putStrLn (printBoard board02)
    -- putStrLn (printBoard board03)
    -- putStrLn (printBoard board04)
    -- putStrLn (printBoard board05)
    -- putStrLn (printBoard board06)
    -- putStrLn (printBoard board07)
    -- putStrLn (printBoard board08)
    -- putStrLn (printBoard board09)
    -- putStrLn (printBoard board10)
    -- putStrLn (printBoard board11)
    -- putStrLn (printBoard board12)
    -- print (computeObjectives board05)
    putStrLn (programCycle board04 15)

board01 = initializeBoard 6 6 876318
-- board01 = initializeBoard 6 6 214
-- board01 = initializeBoard 6 6 134701375
board02 = generateCribs board01 0
board03 = generateObstcles board02 7
board04 = generateKids board03 3
-- board05 = generateRobots board04 3
-- board06 = generateDirts board05 3
-- board07 = doAgentsTurn board06  
-- board08 = doAgentsTurn board07
-- board09 = doAgentsTurn board08
-- board10 = doAgentsTurn board09  
-- board11 = doAgentsTurn board10
-- board12 = doAgentsTurn board11

-- board10 = Operations.moveUp board09 (Robot 2 5 False)
-- board11 = Operations.moveDwn board10 (Robot 1 5 True)
-- board12 = Operations.dropKid board11 (Robot 2 5 True)


programCycle :: Board -> Int -> String
programCycle board 0 = printBoard board ++ "\n" ++ "FINITO"
programCycle board iter =
    let
        newBoard = executeKidsTurn board 
    in
        printBoard board ++ "\n" ++  programCycle newBoard (iter-1)
    