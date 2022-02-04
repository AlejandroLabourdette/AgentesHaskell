module EnviromentTurn where

import Board
import Elements
import ListUtils
import PositionUtils
import Random
import Operations


doEnviromentTurn :: Board -> Board
doEnviromentTurn board = error""

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