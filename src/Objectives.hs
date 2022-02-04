module Objectives where


import Board
import Elements
import ListUtils
import PositionUtils
import Operations


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
        firstStep = computeFirstStep board slot robot
        newObj = Objective robot (x slot) (y slot) objtype firstStep currentCost
        oldLength = lengthBoard board
        oldWidth =  widthBoard board
        oldKids = kids board
        oldRobots = robots board
        oldObstacles = obstacles board
        oldDirts = dirts board
        oldCribs = cribs board
        oldSeed = seed board

removeObjectiveFromBoard :: Board -> Objective -> Board
removeObjectiveFromBoard board obj =
    Board oldLength oldWidth oldKids oldRobots oldObstacles oldDirts oldCribs newObjectives oldSeed 
    where
        newObjectives = ListUtils.remove obj (objectives board)
        oldLength = lengthBoard board
        oldWidth =  widthBoard board
        oldKids = kids board
        oldRobots = robots board
        oldObstacles = obstacles board
        oldDirts = dirts board
        oldCribs = cribs board
        oldSeed = seed board

findObjectiveWithPosGhost :: [Objective] -> Ghost -> Objective 
findObjectiveWithPosGhost [] _ = Objective (Robot 0 0 False) 0 0 falseObj (Ghost 0 0) infinite
findObjectiveWithPosGhost (obj:rest) pos
    | (destinyX obj == x pos) && (destinyY obj == y pos) = obj
    | otherwise = findObjectiveWithPosGhost rest pos 

boardBFS :: Board -> Robot -> Ghost -> [Ghost] -> [(Ghost,Int)] -> Int -> ([Ghost],[(Ghost,Int)])
boardBFS board robot slot visited queue cost
    | loaded robot = 
        let
            (visited1, queue1) = registerSlots1Mov board robot slot visited queue cost
            (visited2, queue2) = registerSlots2Mov board robot slot visited queue cost
            visited3 = ListUtils.join visited1 visited2
            queue3 = ListUtils.join  queue1 queue2
        in
            (visited3,queue3)
    | otherwise = registerSlots1Mov board robot slot visited queue cost
    

registerSlots1Mov :: Board -> Robot -> Ghost -> [Ghost] -> [(Ghost,Int)] -> Int -> ([Ghost],[(Ghost,Int)])
registerSlots1Mov board robot slot visited queue cost =
    let
        imagRobot = Robot (x slot) (y slot) (loaded robot)
        (visited1, queue1) =
            if not (ListUtils.exist upSlot visited) && canMovRobot board imagRobot upDirType 
            then (ListUtils.add upSlot visited, ListUtils.add (upSlot,cost+1) queue)
            else (visited,queue)
        (visited2, queue2) =
            if not (ListUtils.exist dwnSlot visited) && canMovRobot board imagRobot dwnDirType 
            then (ListUtils.add dwnSlot visited1, ListUtils.add (dwnSlot,cost+1) queue1)
            else (visited1,queue1)
        (visited3, queue3) =
            if not (ListUtils.exist lftSlot visited) && canMovRobot board imagRobot lftDirType 
            then (ListUtils.add lftSlot visited2, ListUtils.add (lftSlot,cost+1) queue2)
            else (visited2,queue2)
        (visited4, queue4) =
            if not (ListUtils.exist rghtSlot visited) && canMovRobot board imagRobot rghtDirType 
            then (ListUtils.add rghtSlot visited3, ListUtils.add (rghtSlot,cost+1) queue3)
            else (visited3,queue3)
    in
        (visited4,queue4)
    where
        upSlot = calcPos board slot (-1) 0
        dwnSlot = calcPos board slot 1 0
        lftSlot = calcPos board slot 0 (-1)
        rghtSlot = calcPos board slot 0 1

registerSlots2Mov :: Board -> Robot -> Ghost -> [Ghost] -> [(Ghost,Int)] -> Int -> ([Ghost],[(Ghost,Int)])
registerSlots2Mov board robot slot visited queue cost =
    let
        imagRobot = Robot (x slot) (y slot) (loaded robot)
        (visited1, queue1) =
            if not (ListUtils.exist up2Slot visited) 
                && canMovRobot board imagRobot up2DirType 
            then (ListUtils.add up2Slot visited, ListUtils.add (up2Slot,cost+1) queue)
            else (visited,queue)
        (visited2, queue2) =
            if not (ListUtils.exist dwn2Slot visited)
                && canMovRobot board imagRobot dwn2DirType 
            then (ListUtils.add dwn2Slot visited1, ListUtils.add (dwn2Slot,cost+1) queue1)
            else (visited1,queue1)
        (visited3, queue3) =
            if not (ListUtils.exist lft2Slot visited)
                && canMovRobot board imagRobot lft2DirType 
            then (ListUtils.add lft2Slot visited2, ListUtils.add (lft2Slot,cost+1) queue2)
            else (visited2,queue2)
        (visited4, queue4) =
            if not (ListUtils.exist rght2Slot visited)
                && canMovRobot board imagRobot rght2DirType 
            then (ListUtils.add rght2Slot visited3, ListUtils.add (rght2Slot,cost+1) queue3)
            else (visited3,queue3)
    in
        (visited4,queue4)
    where
        up2Slot = calcPos board slot (-2) 0
        dwn2Slot = calcPos board slot 2 0
        lft2Slot = calcPos board slot 0 (-2)
        rght2Slot = calcPos board slot 0 2

computeFirstStep :: Board -> Ghost -> Robot -> Ghost 
computeFirstStep board endPos robot = _computeFirstStep board robot [endPos] [(endPos,0)]

_computeFirstStep :: Board -> Robot -> [Ghost] -> [(Ghost,Int)] -> Ghost
_computeFirstStep board robot visited [] = error "Robot slot not found in inverse BFS"
_computeFirstStep board robot visited (queueElement:queueRest)
    | closeToObjective board robot slot = slot
    | otherwise =
        let
            (newVisited,newQueue) = boardBFS board robot slot visited queueRest cost
            --  board robot slot visited queueRest cost
        in 
            _computeFirstStep board robot newVisited newQueue
    where
        slot = fst queueElement
        cost = snd queueElement
    
closeToObjective :: Board -> Robot -> Ghost -> Bool 
closeToObjective board robot slot
    | loaded robot = ListUtils.exist robotSlot longList
    | otherwise = ListUtils.exist robotSlot shortList
    where
        robotSlot = Ghost (x robot) (y robot)
        upSlot = calcPos board slot (-1) 0
        up2Slot = calcPos board slot (-2) 0
        dwnSlot = calcPos board slot 1 0
        dwn2Slot = calcPos board slot 2 0
        lftSlot = calcPos board slot 0 (-1)
        lft2Slot = calcPos board slot 0 (-2)
        rghtSlot = calcPos board slot 0 1
        rght2Slot = calcPos board slot 0 2
        shortList = [upSlot,dwnSlot,lftSlot,rghtSlot]
        longList = [upSlot,up2Slot,dwnSlot,dwn2Slot,lftSlot,lft2Slot,rghtSlot,rght2Slot]
