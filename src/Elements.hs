module Elements where

kidType :: [Char]
kidType = "Kid"
robotType :: [Char]
robotType = "Robot"
obstacleType :: [Char]
obstacleType = "Obstacle"
dirtType :: [Char]
dirtType = "Dirt"
cribType :: [Char]
cribType = "Crib"
emptyType :: [Char]
emptyType = "Empty"


class Displayable a where
    x :: a -> Int
    y :: a -> Int
    kind :: a -> [Char]
    loaded :: a -> Bool
    toKid :: a -> Kid
    toRobot :: a -> Robot
    toObstacle :: a -> Obstacle
    toDirt :: a -> Dirt
    toCrib :: a -> Crib
       

data Kid = Kid {
    xKid :: Int,
    yKid :: Int
} deriving (Show, Eq)

instance Displayable Kid where
    x a = xKid a
    y a = yKid a
    kind a = kidType
    loaded a = error "Kids can't be the ones that load"
    toKid a = Kid (xKid a) (yKid a)
    toRobot a = error "Try to convert a Kid in Robot"
    toObstacle a = error "Try to convert a Kid in Obstacle"
    toDirt a = error "Try to convert a Kid in Dirt"
    toCrib a = error "Try to convert a Kid in Crib"


data Robot = Robot {
    xRobot :: Int,
    yRobot :: Int,
    loadedRobot :: Bool
} deriving (Show, Eq)

instance Displayable Robot where
    x a = xRobot a
    y a = yRobot a
    kind a = robotType
    loaded a = loadedRobot a
    toKid a = error "Try to convert a Robot in Kid"
    toRobot a = Robot (xRobot a) (yRobot a) (loadedRobot a)
    toObstacle a = error "Try to convert a Robot in Obstacle"
    toDirt a = error "Try to convert a Robot in Dirt"
    toCrib a = error "Try to convert a Robot in Crib"


data Obstacle = Obstacle {
    xObstacle :: Int,
    yObstacle :: Int
} deriving (Show, Eq)

instance Displayable Obstacle where
    x a = xObstacle a
    y a = yObstacle a
    kind a = obstacleType
    loaded a = error "Obstacles can't be the ones that load"
    toKid a = error "Try to convert a Obstacle in Kid"
    toRobot a = error "Try to convert a Obstacle in Robot"
    toObstacle a = Obstacle (xObstacle a) (yObstacle a)
    toDirt a = error "Try to convert a Robot in Dirt"
    toCrib a = error "Try to convert a Robot in Crib"


data Dirt = Dirt {
    xDirt :: Int,
    yDirt :: Int
} deriving (Show, Eq)

instance Displayable Dirt where
    x a = xDirt a
    y a = yDirt a
    kind a = dirtType
    loaded a = error "Dirts can't be the ones that load"
    toKid a = error "Try to convert a Dirt in Kid"
    toRobot a = error "Try to convert a Dirt in Robot"
    toObstacle a = error "Try to convert a Dirt in Obstacle"
    toDirt a = Dirt (xDirt a) (yDirt a)
    toCrib a = error "Try to convert a Dirt in Crib"


data Crib = Crib {
    xCrib :: Int,
    yCrib :: Int,
    loadedCrib :: Bool
} deriving (Show, Eq)

instance Displayable Crib where
    x a = xCrib a
    y a = yCrib a
    kind a = cribType
    loaded a = loadedCrib a
    toKid a = error "Try to convert a Crib in Kid"
    toRobot a = error "Try to convert a Crib in Robot"
    toObstacle a = error "Try to convert a Crib in Obstacle"
    toDirt a = error "Try to convert a Crib in Dirt"
    toCrib a = Crib (xCrib a) (yCrib a) (loadedCrib a)


data Ghost = Ghost{
    xGhost :: Int,
    yGhost :: Int
}

instance Displayable Ghost where
    x a = xGhost a
    y a = yGhost a
    kind a = emptyType
    loaded a = error "Ghosts can't be the ones that load"
    toKid a = error "Try to convert a Ghost in Kid"
    toRobot a = error "Try to convert a Ghost in Robot"
    toObstacle a = error "Try to convert a Ghost in Obstacle"
    toDirt a = error "Try to convert a Ghost in Dirt"
    toCrib a = error "Try to convert a Ghost in Crib"