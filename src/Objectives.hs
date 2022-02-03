module Objectives where


import Board
import Elements
import ListUtils
import PositionUtils


computeObjectives :: Board -> Board
computeObjectives board = _computeObjectives board (robots board)

_computeObjectives :: Board -> [Robot] -> Board
_computeObjectives board [] = board
_computeObjectives board (robot:rest) = _computeObjectives newBoard rest
    where
        newBoard = computeObjective board robot

computeObjective :: Board -> Robot -> Board
computeObjective board robot
    | existDirt = 
        addObjectiveToBoard board robot (Ghost (x robot) (y robot)) cleanObj 0
    | loaded robot && existCribFree = 
        addObjectiveToBoard board robot (Ghost (x robot) (y robot)) dropObj 0
    | otherwise =
        let
            thisSlot = Ghost (x robot) (y robot)
        in
            _computeObjective board robot [thisSlot] [(thisSlot,0)] 
    where
        possibleDirt = Dirt (x robot) (y robot)
        possibleCrib = Crib (x robot) (y robot) False
        existCribFree = ListUtils.exist possibleCrib (cribs board)
        existDirt = ListUtils.exist possibleDirt (dirts board)

_computeObjective :: Board -> Robot -> [Ghost] -> [(Ghost, Int)] -> Board
_computeObjective board robot visited [] =
    addObjectiveToBoard board robot (Ghost (x robot) (y robot)) noObj 0
_computeObjective board robot visited (queueElement:queueRest)
    | objectives board /= objectives newBoard =
        newBoard
    | otherwise =
        let
            (newVisited,newQueue) = boardBFS newBoard robot currentSlot visited queueRest currentCost 
        in 
            _computeObjective newBoard robot newVisited newQueue
    where
        currentSlot = fst queueElement
        currentCost = snd queueElement
        objType = _computeObjectiveType board robot currentSlot
        newBoard = 
            if objType == falseObj 
            then board
            else addObjectiveToBoard board robot currentSlot objType currentCost

_computeObjectiveType :: Board -> Robot -> Ghost -> [Char]
_computeObjectiveType board robot slot
    | slotType == dirtType = 
        moveObj 
    | loaded robot && slotType == cribType =
        moveObj
    | not(loaded robot) && slotType == kidType && noCrib && noRobot =
        moveObj 
    | otherwise =
        falseObj
    where
        slotType = get board (x slot) (y slot)
        possibleCrib = Crib (x slot) (y slot) True
        possibleRobot = Robot (x slot) (y slot)
        noCrib = not(ListUtils.exist possibleCrib (cribs board))
        noRobot = not(ListUtils.exist (possibleRobot True) (robots board)) &&
            not(ListUtils.exist (possibleRobot False) (robots board))

addObjectiveToBoard :: Board -> Robot -> Ghost -> [Char] -> Int -> Board
addObjectiveToBoard board robot slot objtype currentCost
    | currentCost < cost oldObj = 
        let
            cleanObjectives = ListUtils.remove oldObj (objectives board)
            newObjectives = ListUtils.add newObj cleanObjectives
            newBoard = Board oldLength oldWidth oldKids oldRobots oldObstacles oldDirts oldCribs newObjectives oldSeed
        in
            if action oldObj == falseObj 
            then newBoard
            else computeObjective newBoard (robotObj oldObj)
    | otherwise = board
    where
        oldObj = findObjectiveWithPosGhost (objectives board) slot
        newObj = Objective robot (x slot) (y slot) objtype currentCost
        oldLength = lengthBoard board
        oldWidth =  widthBoard board
        oldKids = kids board
        oldRobots = robots board
        oldObstacles = obstacles board
        oldDirts = dirts board
        oldCribs = cribs board
        oldSeed = seed board

findObjectiveWithPosGhost :: [Objective] -> Ghost -> Objective 
findObjectiveWithPosGhost [] _ = Objective (Robot 0 0 False) 0 0 falseObj infinite
findObjectiveWithPosGhost (obj:rest) pos
    | (destinyX obj == x pos) && (destinyY obj == y pos) = obj
    | otherwise = findObjectiveWithPosGhost rest pos 

boardBFS :: Board -> Robot -> Ghost -> [Ghost] -> [(Ghost,Int)] -> Int -> ([Ghost],[(Ghost,Int)])
boardBFS board robot slot visited queue cost
    | loaded robot = registerSlots board toRegisterLong visited queue cost
    | otherwise = registerSlots board toregisterShort visited queue cost
    where
        upSlot = calcPos board slot (-1) 0
        up2Slot = calcPos board slot (-2) 0
        dwnSlot = calcPos board slot 1 0
        dwn2Slot = calcPos board slot 2 0
        lftSlot = calcPos board slot 0 (-1)
        lft2Slot = calcPos board slot 0 (-2)
        rghtSlot = calcPos board slot 0 1
        rght2Slot = calcPos board slot 0 2
        toregisterShort = [upSlot,dwnSlot,lftSlot,rghtSlot]
        toRegisterLong = [upSlot,up2Slot,dwnSlot,dwn2Slot,lftSlot,lft2Slot,rghtSlot,rght2Slot]

registerSlots :: Board -> [Ghost] -> [Ghost] -> [(Ghost,Int)] -> Int -> ([Ghost],[(Ghost,Int)])
registerSlots board [] visited queue cost = (visited,queue)
registerSlots board (toReg:rest) visited queue cost
    | not (ListUtils.exist toReg visited) && slotTypeIsCorrect =
        let
            newVisited = ListUtils.add toReg visited
            newQueue = ListUtils.add (toReg,cost+1) queue
        in
            registerSlots board rest newVisited newQueue cost
    | otherwise = 
        registerSlots board rest visited queue cost
    where
        slotType = get board (x toReg) (y toReg)
        slotTypeIsCorrect =
            slotType == kidType || -- are kids fine here?
            slotType == dirtType || 
            slotType == cribType || 
            slotType == emptyType