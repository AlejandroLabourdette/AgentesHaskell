module AgentsTurn where

import Board
import Objectives
import Elements
import PositionUtils
import ListUtils
import Operations 


doAgentsTurn :: Board -> Board
doAgentsTurn board =
    let
        objectivesPurgedBoard = purgeField board objectivesField
        newBoard = computeObjectives objectivesPurgedBoard
    in
        _doAgentsTurn newBoard (robots newBoard)

_doAgentsTurn :: Board -> [Robot] -> Board
_doAgentsTurn = foldl _doAgentTurn

_doAgentTurn :: Board -> Robot -> Board
_doAgentTurn board robot
    | action newobjective == moveObj =
        executeMovObj newBoard2 newobjective
    | action newobjective == cleanObj =
        executeCleanObj newBoard2 newobjective
    | action newobjective == dropObj =
        executeDropObj newBoard2 newobjective
    | action newobjective == noObj =
        newBoard2
    | otherwise = error("Faltan Por implementar " ++ action newobjective)
    where
        objective = getRobotObjective (objectives board) robot
        newBoard1 = removeObjectiveFromBoard board objective
        newBoard2 = computeObjective newBoard1 robot
        newobjective = getRobotObjective (objectives newBoard2) robot

getRobotObjective :: [Objective] -> Robot -> Objective
getRobotObjective [] robot = error "Objective not found"
getRobotObjective (obj:rest) robot =
    if robotObj obj == robot
    then obj
    else getRobotObjective rest robot

executeMovObj :: Board -> Objective -> Board 
executeMovObj board objective
    | directionType == upDirType =
        moveUp board robot
    | directionType == up2DirType =
        moveUpDouble board robot
    | directionType == dwnDirType =
        moveDwn board robot
    | directionType == dwn2DirType =
        moveDwnDouble board robot
    | directionType == lftDirType =
        moveLft board robot
    | directionType == lft2DirType =
        moveLftDouble board robot
    | directionType == rghtDirType =
        moveRght board robot
    | directionType == rght2DirType =
        moveRghtDouble board robot
    | otherwise = error "Direction Type pattern not matched"
    where
        robot = robotObj objective
        destiny = Ghost (x (nextSlot objective)) (y (nextSlot objective))
        directionType = getDirectionType robot destiny

executeCleanObj :: Board -> Objective -> Board
executeCleanObj board obj = clean board robot
    where
        robot = robotObj obj

executeDropObj :: Board -> Objective -> Board
executeDropObj board obj = dropKid board robot
    where
        robot = robotObj obj