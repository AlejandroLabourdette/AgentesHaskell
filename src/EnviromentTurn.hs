module EnviromentTurn where

import Board
import Elements
import ListUtils
import PositionUtils
import Random
import Operations


doEnviromentTurn :: Board -> Board
doEnviromentTurn board = error""


executeKidsTurn :: Board -> Board 
executeKidsTurn board = 
    _executeKidsTurn board notLoadedKids
    where
        notLoadedKids = getKidsNotLoaded board (kids board)

getKidsNotLoaded :: Board -> [Kid] -> [Kid]
getKidsNotLoaded board [] = []
getKidsNotLoaded board (kid:rest)
    | existRobot || existCrib = 
        getKidsNotLoaded board rest
    | otherwise =
        ListUtils.add kid (getKidsNotLoaded board rest)
    where
        possibleRobot = Robot (x kid) (y kid) True 
        possibleCrib = Crib (x kid) (y kid) True 
        existRobot = ListUtils.exist possibleRobot (robots board)
        existCrib = ListUtils.exist possibleCrib (cribs board)

_executeKidsTurn :: Board -> [Kid] -> Board 
_executeKidsTurn board [] = board
_executeKidsTurn board (kid:rest) = 
    _executeKidsTurn createdDirtBoard noExecutedKids
    where
        grid = getGridForKid board kid
        currentKids = ListUtils.add kid rest
        kidsInGrid = getKidsInGrid currentKids grid
        movedKidsBoard = moveKidsRandom board kidsInGrid 
        noExecutedKids = ListUtils.removeList kidsInGrid currentKids
        slotsForDirt = slotsAvaibleForDirt movedKidsBoard grid
        dirtCont = amountDirtToGenerate (length kidsInGrid)
        createdDirtBoard = createRandomDirtInSlots movedKidsBoard slotsForDirt dirtCont


getKidsInGrid :: [Kid] -> [Ghost] -> [Kid]
getKidsInGrid [] grid = []
getKidsInGrid (nextKid:rest) grid =
    if exist nextKidSlot grid
    then ListUtils.add nextKid (getKidsInGrid rest grid)
    else getKidsInGrid rest grid
    where
        nextKidSlot = Ghost (x nextKid) (y nextKid)
        

getGridForKid :: Board -> Kid -> [Ghost]
getGridForKid board kid =
    selectedGrid -- seed is not changed
    where
        slot =  Ghost (x kid) (y kid)
        grids = possiblesGrid board slot
        i = generateRandom (length grids) (seed board)
        selectedGrid = ListUtils.index grids i
        newBoard = changeSeed board

possiblesGrid :: Board -> Ghost -> [[Ghost]]
possiblesGrid board slot = 
    _possiblesGrid board slots
    where
        slot1 = calcPos board slot (-1) (-1)
        slot2 = calcPos board slot (-1) 0
        slot3 = calcPos board slot (-1) 1
        slot4 = calcPos board slot 0 (-1)
        slot6 = calcPos board slot 0 1
        slot7 = calcPos board slot 1 (-1)
        slot8 = calcPos board slot 1 0
        slot9 = calcPos board slot 1 1        
        slots = [slot,slot1,slot2,slot3,slot4,slot6,slot7,slot8,slot9]

_possiblesGrid :: Board -> [Ghost] -> [[Ghost]]
_possiblesGrid board [] = []
_possiblesGrid board (center:rest) =
    if length grid == 9
    then ListUtils.add grid (_possiblesGrid board rest)
    else _possiblesGrid board rest
    where
        grid = gridWithCenter board center

gridWithCenter :: Board -> Ghost -> [Ghost]
gridWithCenter board center=
    addListWithoutRepetition slots [center]
    where
        slot1 = calcPos board center (-1) (-1)
        slot2 = calcPos board center (-1) 0
        slot3 = calcPos board center (-1) 1
        slot4 = calcPos board center 0 (-1)
        slot6 = calcPos board center 0 1
        slot7 = calcPos board center 1 (-1)
        slot8 = calcPos board center 1 0
        slot9 = calcPos board center 1 1
        slots = [slot1,slot2,slot3,slot4,slot6,slot7,slot8,slot9]


moveKidsRandom :: Board -> [Kid] -> Board 
moveKidsRandom = foldl moveKidRandom

moveKidRandom :: Board -> Kid -> Board 
moveKidRandom board kid
    | direction == upDirType =
        moveUp newBoard kid
    | direction == dwnDirType =
        moveDwn newBoard kid
    | direction == lftDirType =
        moveLft newBoard kid
    | direction == rghtDirType =
        moveRght newBoard kid
    | otherwise =
        error "Move kid random with unknow direction"
    where
        upSlot = calcPos board kid (-1) 0
        dwnSlot = calcPos board kid 1 0
        lftSlot = calcPos board kid 0 (-1)
        rghtSlot = calcPos board kid 0 1
        allSlots = [upSlot, dwnSlot, lftSlot, rghtSlot]
        slotsAvaible = slotsAvaibleForKid board allSlots
        i = generateRandom (length slotsAvaible) (seed board)
        selectedSlot = ListUtils.index slotsAvaible i
        direction = getDirectionType kid selectedSlot
        newBoard = changeSeed board

slotsAvaibleForKid :: Board -> [Ghost] -> [Ghost] 
slotsAvaibleForKid board [] = []
slotsAvaibleForKid board (slot:rest)
    | slotType == emptyType || slotType == obstacleType =
        ListUtils.add slot (slotsAvaibleForKid board rest)
    | otherwise =
        slotsAvaibleForKid board rest
    where
        slotType = get board (x slot) (y slot)

slotsAvaibleForDirt :: Board -> [Ghost] -> [Ghost]
slotsAvaibleForDirt board [] = []
slotsAvaibleForDirt board (slot:rest)
    | slotType == emptyType =
        ListUtils.add slot (slotsAvaibleForDirt board rest)
    | otherwise =
        slotsAvaibleForDirt board rest
    where
        slotType = get board (x slot) (y slot)

createRandomDirtInSlots :: Board -> [Ghost] -> Int -> Board 
createRandomDirtInSlots board [] cant = board
createRandomDirtInSlots board slots 0 = board
createRandomDirtInSlots board slots cant
    | i == 0 =
        createRandomDirtInSlots newSeedBoard slots (cant-1)
    | otherwise =
        let
            selectedSlot = ListUtils.index slots i
            newSlots = ListUtils.remove selectedSlot slots
            dirt = Dirt (x selectedSlot) (y selectedSlot)
            addedDirtBoard = Board.add newSeedBoard dirt
        in
            createRandomDirtInSlots addedDirtBoard newSlots (cant-1)
    where
        i = generateRandom (length slots+1) (seed board) -1
        newSeedBoard = changeSeed board
        
amountDirtToGenerate :: Int -> Int
amountDirtToGenerate amountKids
    | amountKids == 1 = 1
    | amountKids == 2 = 3
    | otherwise = 6