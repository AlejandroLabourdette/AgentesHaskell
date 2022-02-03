module Objectives where


import Board
import Elements


computeObjectives :: Board -> Board
computeObjectives board = _computeObjectives board (robots board)

_computeObjectives :: Board -> [Robot] -> Board 
_computeObjectives board [] = board
_computeObjectives board (robot:rest) = error "Not implemented"