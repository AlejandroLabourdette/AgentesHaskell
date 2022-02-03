module ListUtils where


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

index :: [a] -> Int -> a
index [] i = error "Index out of range"
index (e:r) 1 = e
index (e:r) i = index r (i-1)