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
import Control.Concurrent
import GHC.Float

-- Simulation Config
n = 7
m = 7
mySeed = 2133
kidsNum = 3
robotsNum = 3 
obstaclesNum = 3
dirtsNum = 3
dirtsNumEnvTurn = 3
enviromentTurnFrequency = 5
iterations = 1000000000
stepByStep = False


main :: IO ()
main = do
    printCycle board06 iterations
    -- clearScreen

board01 = initializeBoard n m mySeed
board02 = generateCribs board01 kidsNum
board03 = generateObstcles board02 obstaclesNum
board04 = generateKids board03 kidsNum
board05 = generateRobots board04 robotsNum
board06 = generateDirts board05 dirtsNum

clearScreen :: IO ()
clearScreen = do
    putStr "\ESC[2J"

printCycle :: Board -> Int -> IO ()
printCycle board 0 = do
    putStrLn (boardToString board)
    putStrLn "Done!"
printCycle board iter
    | isSimulationLost newBoard2 = do
        putStrLn (boardToString newBoard2)
        putStrLn "Too much dirts!"
        printCycle newBoard2 0
    | isSimulationWon newBoard2 = do
        putStrLn (boardToString newBoard2)
        putStrLn "Robots win: all kids are loaded!"
        printCycle newBoard2 0
    | otherwise = do
        putStrLn (boardToString newBoard2)
        sleep
        printCycle newBoard2 (iter-1)
    where
        newBoard = doAgentsTurn board
        newBoard2 =
            if  mod iter enviromentTurnFrequency == 0 
            -- if True 
            then doEnviromentTurn newBoard dirtsNumEnvTurn
            else newBoard

sleep :: IO ()
sleep =
    if stepByStep
    then threadDelay 3000000
    else putStr ""


isSimulationLost :: Board -> Bool
isSimulationLost board
    | percent > 0.3 = True
    | otherwise = False
    where
        emptySlots = int2Float(length (getAllEmptySlots board))
        dirtSlots = int2Float(length (dirts board))
        sum = emptySlots + dirtSlots
        percent = dirtSlots / sum
    
isSimulationWon :: Board -> Bool
isSimulationWon board = _isSimulationWon board (kids board)

_isSimulationWon :: Board -> [Kid] -> Bool
_isSimulationWon board [] = True
_isSimulationWon board (kid:rest)
    | existCrib || existRobot =
        _isSimulationWon board rest
    | otherwise =
        False
    where
        possibleRobot = Robot (x kid) (y kid) True
        possibleCrib = Crib (x kid) (y kid) True
        existRobot = ListUtils.exist possibleRobot (robots board)
        existCrib = ListUtils.exist possibleCrib (cribs board)