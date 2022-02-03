module Generate where


import Random
import Board
import Elements
import ListUtils
import Operations


generateCribs :: Board -> Int -> Board
generateCribs board cant = _generateCribs board cant []

_generateCribs :: Board -> Int -> [(Int,Int)] -> Board
_generateCribs board 0 cribs = board
_generateCribs board cant cribsPos =
    let
        freeSlots = getAvaibleSlots board cribsPos
        freeSlotsAmount = length freeSlots
        i = generateRandom freeSlotsAmount (seed board)
        newSeedBoard = changeSeed board
        selectedSlot = ListUtils.index freeSlots i
        newCribsPos = ListUtils.add selectedSlot cribsPos
        crib = uncurry Crib selectedSlot False
        newBoard = Board.add newSeedBoard crib
    in
        _generateCribs newBoard (cant-1) newCribsPos

generateKids :: Board -> Int -> Board
generateKids board 0 = board
generateKids board cant =
    let
        freeSlots = getAllEmptySlots board
        freeSlotsAmount = length freeSlots
        i = generateRandom freeSlotsAmount (seed board)
        newSeedBoard = changeSeed board
        selectedSlot = ListUtils.index freeSlots i
        kid = uncurry Kid selectedSlot
        newBoard = Board.add newSeedBoard kid
    in
        generateKids newBoard (cant-1)

generateRobots :: Board -> Int -> Board
generateRobots board 0 = board
generateRobots board cant =
    let
        freeSlots = getAllEmptySlots board
        freeSlotsAmount = length freeSlots
        i = generateRandom freeSlotsAmount (seed board)
        newSeedBoard = changeSeed board
        selectedSlot = ListUtils.index freeSlots i
        robot = uncurry Robot selectedSlot False
        newBoard = Board.add newSeedBoard robot
    in
        generateRobots newBoard (cant-1)

generateObstcles :: Board -> Int -> Board
generateObstcles board 0 = board
generateObstcles board cant =
    let
        freeSlots = getAllEmptySlots board
        freeSlotsAmount = length freeSlots
        i = generateRandom freeSlotsAmount (seed board)
        newSeedBoard = changeSeed board
        selectedSlot = ListUtils.index freeSlots i
        obstacle = uncurry Obstacle selectedSlot
        newBoard = Board.add newSeedBoard obstacle
    in
        generateObstcles newBoard (cant-1)

generateDirts :: Board -> Int -> Board
generateDirts board 0 = board
generateDirts board cant =
    let
        freeSlots = getAllEmptySlots board
        freeSlotsAmount = length freeSlots
        i = generateRandom freeSlotsAmount (seed board)
        newSeedBoard = changeSeed board
        selectedSlot = ListUtils.index freeSlots i
        dirt = uncurry Dirt selectedSlot
        newBoard = Board.add newSeedBoard dirt
    in
        generateObstcles newBoard (cant-1)


getAvaibleSlots :: Board -> [(Int,Int)] -> [(Int,Int)]
getAvaibleSlots board [] = getAllEmptySlots board
getAvaibleSlots board adyacentTo = getAdyEmptySlots board adyacentTo

getAllEmptySlots :: Board -> [(Int,Int)]
getAllEmptySlots board = _getAllEmptySlotsR board n m []
    where
        n = lengthBoard board
        m = widthBoard board

_getAllEmptySlotsR :: Board -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
_getAllEmptySlotsR board 0 m emptys = emptys
_getAllEmptySlotsR board n 0 emptys = _getAllEmptySlotsR board (n-1) width emptys
    where
        width = widthBoard board
_getAllEmptySlotsR board n m emptys
    | elementType == emptyType =
        let
            newEmptys = ListUtils.add (x,y) emptys
        in
            _getAllEmptySlotsR board n (m-1) newEmptys
    | otherwise = _getAllEmptySlotsR board n (m-1) emptys
    where
        x = lengthBoard board - (n-1)
        y = widthBoard board - (m-1)
        elementType = get board x y

getAdyEmptySlots :: Board -> [(Int,Int)] -> [(Int,Int)]
getAdyEmptySlots board positions = _getAdyEmptySlotsR board positions []

_getAdyEmptySlotsR :: Board -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
_getAdyEmptySlotsR board [] selected = selected
_getAdyEmptySlotsR board ((x,y):r) selected =
    let
        newSelected = ListUtils.join ady selected
    in
        _getAdyEmptySlotsR board r newSelected
    where
        slot = Ghost x y
        p1 = (_calcPosX board slot 1, _calcPosY board slot 0)
        p2 = (_calcPosX board slot (-1), _calcPosY board slot 0)
        p3 = (_calcPosX board slot 0, _calcPosY board slot 1)
        p4 = (_calcPosX board slot 0, _calcPosY board slot (-1))
        emptys = _filterEmpty board [p1,p2,p3,p4] []
        ady = ListUtils.remove (x,y) emptys -- In case i'm in


_filterEmpty :: Board -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
_filterEmpty board [] emptys = emptys
_filterEmpty board ((x,y):r) emptys
    | elementType == emptyType = _filterEmpty board r (ListUtils.add (x,y) emptys)
    | otherwise = _filterEmpty board r emptys
    where
        elementType = get board x y