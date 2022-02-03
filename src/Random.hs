module Random where
import Board

-- change the seed after use it
changeSeed :: Board -> Board
changeSeed b = Board (lengthBoard b) (widthBoard b) (kids b) (robots b) (obstacles b) (dirts b) (cribs b) newSeed
    where
        oldSeed = seed b
        newSeed = _generateForSeed oldSeed

_generateForSeed :: Int -> Int
_generateForSeed seed = mod generated 10000000 
    where
        generated = div (seed * 79658437 * (seed + 9687)) (87 * seed)

generateRandom :: Int -> Int -> Int
generateRandom max sem = abs (mod sem max) +1
