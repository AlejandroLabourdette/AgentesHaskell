module ListUtils where
import GHC.IO.Handle (noNewlineTranslation)


exist :: Eq a => a -> [a] -> Bool
exist e [] = False
exist e (x:xs)
    | e == x = True
    | otherwise = exist e xs

add :: a -> [a] -> [a]
add element list = list ++ [element]

join :: Eq a => [a] -> [a] -> [a]
join [] list = list
join (e:r) list
    | exist e list = join r list
    | otherwise = join r (add e list)

remove :: Eq a => a -> [a] -> [a]
remove e [] = []
remove e (x:xs)
    | e == x = xs
    | otherwise = x : remove e xs

removeList :: Eq a => [a] -> [a] -> [a]
removeList [] list = []
removeList (element:rest) list =
    removeList rest newList
    where
        newList = remove element list

index :: [a] -> Int -> a
index [] i = error ("Index out of range" ++show i)
index (e:r) 1 = e
index (e:r) i = index r (i-1)

addWithoutRepetition :: Eq a => a -> [a] -> [a]
addWithoutRepetition elem list
    | exist elem list = list
    | otherwise = add elem list

addListWithoutRepetition :: Eq a => [a] -> [a] -> [a]
addListWithoutRepetition rest destiny
  = foldl (flip addWithoutRepetition) destiny rest