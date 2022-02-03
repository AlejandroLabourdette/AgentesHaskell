module Board where

import Elements
import ListUtils


data Board = Board {
    lengthBoard :: Int,
    widthBoard :: Int,
    kids :: [Kid],
    robots :: [Robot],
    obstacles :: [Obstacle],
    dirts :: [Dirt],
    cribs :: [Crib],
    objectives :: [Objective],
    seed :: Int
} deriving (Show)

data Objective = Objective {
    robotObj :: Robot,
    destinyX :: Int,
    destinyY :: Int,
    action :: String,
    nextSlot :: Ghost,
    cost :: Int
} deriving (Show, Eq)

cleanObj = "Clean"
dropObj = "Drop"
moveObj = "Move"
noObj = "No"
falseObj = "False"

initializeBoard :: Int -> Int -> Int -> Board
initializeBoard n m = Board n m [] [] [] [] [] []


get :: Board -> Int -> Int -> [Char]
get board x y
    | exist (Kid x y) (kids board) = kidType
    | exist (Robot x y True) (robots board)
        || exist (Robot x y False) (robots board) = robotType
    | exist (Obstacle x y) (obstacles board) = obstacleType 
    | exist (Dirt x y) (dirts board) = dirtType 
    | exist (Crib x y True) (cribs board)
        || exist (Crib x y False) (cribs board) = cribType
    | otherwise = emptyType


add :: Displayable a => Board -> a -> Board 
add board element
    | kind element == kidType = 
        let 
            newKids = ListUtils.add (toKid element) (kids board)
        in
            Board oldLength oldWidth newKids oldRobots oldObstacles oldDirts oldCribs oldObjectives oldSeed
    | kind element == robotType = 
        let
            newRobots = ListUtils.add (toRobot element) (robots board)
        in
            Board oldLength oldWidth oldKids newRobots oldObstacles oldDirts oldCribs oldObjectives oldSeed
    | kind element == obstacleType =
        let
            newObstacles = ListUtils.add (toObstacle element) (obstacles board)
        in
            Board oldLength oldWidth oldKids oldRobots newObstacles oldDirts oldCribs oldObjectives oldSeed
    | kind element == dirtType  =
        let
            newDirts = ListUtils.add (toDirt element) (dirts board)
        in
            Board oldLength oldWidth oldKids oldRobots oldObstacles newDirts oldCribs oldObjectives oldSeed
    | kind element == cribType  =
        let
            newCribs = ListUtils.add (toCrib element) (cribs board)
        in
            Board oldLength oldWidth oldKids oldRobots oldObstacles oldDirts newCribs oldObjectives oldSeed
    | otherwise = error "Element not binded to any type"
    where
        oldLength = lengthBoard board
        oldWidth = widthBoard board
        oldKids = kids board
        oldRobots = robots board
        oldObstacles = obstacles board
        oldDirts = dirts board
        oldCribs = cribs board
        oldObjectives = objectives board
        oldSeed = seed board


remove :: Displayable a => Board -> a -> Board
remove board element
    | kind element == kidType = 
        let 
            newKids = ListUtils.remove (toKid element) (kids board)
        in
            Board oldLength oldWidth newKids oldRobots oldObstacles oldDirts oldCribs oldObjectives oldSeed
    | kind element == robotType = 
        let
            newRobots = ListUtils.remove (toRobot element) (robots board)
        in
            Board oldLength oldWidth oldKids newRobots oldObstacles oldDirts oldCribs oldObjectives oldSeed
    | kind element == obstacleType =
        let
            newObstacles = ListUtils.remove (toObstacle element) (obstacles board)
        in
            Board oldLength oldWidth oldKids oldRobots newObstacles oldDirts oldCribs oldObjectives oldSeed
    | kind element == dirtType  =
        let
            newDirts = ListUtils.remove (toDirt element) (dirts board)
        in
            Board oldLength oldWidth oldKids oldRobots oldObstacles newDirts oldCribs oldObjectives oldSeed
    | kind element == cribType  =
        let
            newCribs = ListUtils.remove (toCrib element) (cribs board)
        in
            Board oldLength oldWidth oldKids oldRobots oldObstacles oldDirts newCribs oldObjectives oldSeed
    | otherwise = error "Element not binded to any type"
    where
        oldLength = lengthBoard board
        oldWidth = widthBoard board
        oldKids = kids board
        oldRobots = robots board
        oldObstacles = obstacles board
        oldDirts = dirts board
        oldCribs = cribs board
        oldObjectives = objectives board
        oldSeed = seed board

purgeField :: Board -> [Char] -> Board
purgeField board field
    | field == objectivesField = 
        Board oldLength oldWidth oldKids oldRobots oldObstacles oldDirts oldCribs [] oldSeed
    | otherwise = error "Element not binded to any type"
    where
        oldLength = lengthBoard board
        oldWidth = widthBoard board
        oldKids = kids board
        oldRobots = robots board
        oldObstacles = obstacles board
        oldDirts = dirts board
        oldCribs = cribs board
        oldObjectives = objectives board
        oldSeed = seed board

objectivesField = "Objectives"