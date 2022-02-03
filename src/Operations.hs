module Operations where

import ListUtils
import PositionUtils
import Board
import Elements


grabKid :: Board -> Robot -> Board
grabKid board robot =
    let
        newRobot = Robot (x robot) (y robot) True
        newBoard = Board.remove board robot
    in 
        Board.add newBoard newRobot
        

dropKid :: Board -> Robot -> Board 
dropKid board robot 
    | existEmptyCrib =
        let
            newCrib = Crib (x robot) (y robot) True 
            boardWithoutCrib = Board.remove newBoard possibleCrib
            boardReady = Board.add boardWithoutCrib newCrib
        in
            Board.add boardReady newRobot
    | otherwise =
        Board.add newBoard newRobot
    where
        newRobot = Robot (x robot) (y robot) False
        newBoard = Board.remove board robot
        possibleCrib = Crib (x robot) (y robot) False
        existEmptyCrib = ListUtils.exist possibleCrib (cribs newBoard)

clean :: Board -> Robot -> Board
clean board robot = Board.remove board dirt 
    where
        dirt = Dirt (x robot) (y robot)


moveUp :: Displayable a => Board -> a -> Board
moveUp board element = _move board element (-1) 0
moveUpDouble :: Displayable a => Board -> a -> Board
moveUpDouble board element = _moveDouble board element (-1) 0
moveDwn :: Displayable a => Board -> a -> Board
moveDwn board element = _move board element 1 0
moveDwnDouble :: Displayable a => Board -> a -> Board
moveDwnDouble board element = _moveDouble board element 1 0
moveLft :: Displayable a => Board -> a -> Board
moveLft board element = _move board element 0 (-1)
moveLftDouble :: Displayable a => Board -> a -> Board
moveLftDouble board element = _moveDouble board element 0 (-1)
moveRght :: Displayable a => Board -> a -> Board
moveRght board element = _move board element 0 1
moveRghtDouble :: Displayable a => Board -> a -> Board
moveRghtDouble board element = _moveDouble board element 0 1


_move :: Displayable a => Board -> a -> Int -> Int -> Board
_move board element directionX directionY
    | kind element == kidType = _moveKid board (toKid element) directionX directionY
    | kind element == robotType = _moveRobot board (toRobot element) directionX directionY
    | otherwise = error "Elemenent not movable"

_moveDouble :: Displayable a => Board -> a -> Int -> Int -> Board
_moveDouble board element directionX directionY
    | (kind element == robotType) && loaded element = 
        let 
            newBoard = _move board element directionX directionY
        in 
            _move newBoard element directionX directionY
    | otherwise = error "Only can do double movement robots that have a child loaded"

_moveKid :: Board -> Kid -> Int -> Int -> Board 
_moveKid board kid directionX directionY
    | exist obstacle (obstacles board) =
        let
            newBoard = _moveObstacle board obstacle directionX directionY
        in
            _moveKidForce newBoard kid directionX directionY
    | exist loadedRobot (robots board) = board
    | otherwise = 
        _moveKidForce board kid directionX directionY
    where
        posX = calcPosX board kid directionX 
        posY = calcPosY board kid directionY
        obstacle = Obstacle posX posY
        loadedRobot = Robot (x kid) (y kid) True

_moveKidForce :: Board -> Kid -> Int -> Int -> Board 
_moveKidForce board kid directionX directionY
    | elementType == emptyType =
        let
            newBoard = Board.remove board kid
        in
            Board.add newBoard newKid
    | otherwise = board
    where 
        posX = calcPosX board kid directionX
        posY = calcPosY board kid directionY
        elementType = get board posX posY
        newKid = Kid posX posY

_moveObstacle :: Board -> Obstacle -> Int -> Int -> Board 
_moveObstacle board obstacle directionX directionY
    | obstacle == otherObstacle = board
    | exist otherObstacle (obstacles board) = 
        let 
            newBoard = _moveObstacle board otherObstacle directionX directionY
        in 
            _moveObstacleForce newBoard obstacle directionX directionY
    | otherwise = _moveObstacleForce board obstacle directionX directionY
    where
        posX = calcPosX board obstacle directionX 
        posY = calcPosY board obstacle directionY
        otherObstacle = Obstacle posX posY

_moveObstacleForce :: Board -> Obstacle  -> Int -> Int -> Board 
_moveObstacleForce board obstacle directionX directionY
    | elementType == emptyType =
        let
            newBoard = Board.remove board obstacle
        in
            Board.add newBoard newObstacle
    | otherwise = board
    where
        posX = calcPosX board obstacle directionX 
        posY = calcPosY board obstacle directionY
        elementType = get board posX posY
        newObstacle = Obstacle posX posY

_moveRobot :: Board -> Robot -> Int -> Int -> Board 
_moveRobot board robot directionX directionY
    | nextPosType == kidType =
        if loaded robot || ListUtils.exist (Robot posX posY True) (robots board)
        then board
        else 
            let 
                newBoard = Board.remove board robot
            in
                Board.add newBoard (Robot posX posY True)
    | nextPosType == dirtType || nextPosType == cribType || nextPosType == emptyType =
        if loaded robot
        then
            let
                new1Board = Board.remove board (Kid (x robot) (y robot))
                new2Board = Board.remove new1Board robot
                new3Board = Board.add new2Board (Kid posX posY)
            in
                Board.add new3Board (Robot posX posY (loaded robot))
        else 
            let
                newBoard = Board.remove board robot
            in
                Board.add newBoard (Robot posX posY (loaded robot))
    | otherwise = board
    where
        posX = calcPosX board robot directionX 
        posY = calcPosY board robot directionY
        nextPosType = get board posX posY


canMovRobot :: Board -> Robot -> [Char] -> Bool
canMovRobot board robot movDirType
    | movDirType == upDirType ||
        movDirType == dwnDirType ||
        movDirType == lftDirType ||
        movDirType == rghtDirType =
            _canSimpleMoveRobot board robot movDirType
    | otherwise =
        _canDoubleMoveRobot board robot movDirType

_canSimpleMoveRobot :: Board -> Robot -> [Char] -> Bool 
_canSimpleMoveRobot board robot movDirType 
    | movDirType == upDirType =
        let newBoard = moveUp board robot
        in not(ListUtils.exist robot (robots newBoard))
    | movDirType == dwnDirType =
        let newBoard = moveDwn board robot
        in not(ListUtils.exist robot (robots newBoard))
    | movDirType == rghtDirType =
        let newBoard = moveRght board robot
        in not(ListUtils.exist robot (robots newBoard))
    | movDirType == lftDirType =
        let newBoard = moveLft board robot
        in not(ListUtils.exist robot (robots newBoard))
    | otherwise = error "Not movDirType recognize in canSimpleMove"

_canDoubleMoveRobot :: Board -> Robot -> [Char] -> Bool 
_canDoubleMoveRobot board robot movDirType 
    | movDirType == up2DirType =
        let 
            can1Move = _canSimpleMoveRobot board robot upDirType
            newBoard = moveUp board robot
            newPos = calcPos newBoard robot (-1) 0
            newRobot = _findRobotAt (robots newBoard) newPos
            can2Move = _canSimpleMoveRobot newBoard newRobot upDirType
        in
            can1Move && can2Move
    | movDirType == dwn2DirType =
        let 
            can1Move = _canSimpleMoveRobot board robot dwnDirType
            newBoard = moveDwn board robot
            newPos = calcPos newBoard robot 1 0
            newRobot = _findRobotAt (robots newBoard) newPos
            can2Move = _canSimpleMoveRobot newBoard newRobot dwnDirType
        in
            can1Move && can2Move
    | movDirType == rght2DirType =
        let 
            can1Move = _canSimpleMoveRobot board robot rghtDirType
            newBoard = moveRght board robot
            newPos = calcPos newBoard robot 0 1
            newRobot = _findRobotAt (robots newBoard) newPos
            can2Move = _canSimpleMoveRobot newBoard newRobot rghtDirType
        in
            can1Move && can2Move
    | movDirType == lft2DirType =
        let 
            can1Move = _canSimpleMoveRobot board robot lftDirType
            newBoard = moveLft board robot
            newPos = calcPos newBoard robot 0 (-1)
            newRobot = _findRobotAt (robots newBoard) newPos
            can2Move = _canSimpleMoveRobot newBoard newRobot lftDirType
        in
            can1Move && can2Move
    | otherwise = error "Not movDirType recognize in canDoubleMove"

_findRobotAt :: Displayable a => [Robot] -> a -> Robot
_findRobotAt [] pos = error "Robot not founded"
_findRobotAt (robot:rest) pos
    | x robot == x pos && y robot == y pos = 
        robot
    | otherwise = _findRobotAt rest pos