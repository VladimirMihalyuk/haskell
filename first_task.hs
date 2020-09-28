factorial :: Int -> Integer
factorial n
    | n == 0    = 1
    | otherwise = (toInteger n) * (factorial $n - 1)

factorialHelper :: Int -> Int -> Integer
factorialHelper n a
    | n == 0    = toInteger a
    | otherwise = factorialHelper (n - 1) (n * a)

factorialTail :: Int -> Integer
factorialTail n = factorialHelper n 1

fib :: Int -> Integer
fib n
    | n == 0    = 0
    | n == 1    = 1
    | otherwise = fib(n - 1) + fib(n - 2)

fibHelper :: Int -> Int -> Int -> Integer
fibHelper n a b
    | n == 0    = toInteger a
    | otherwise = fibHelper (n - 1) b (a + b)

fibTail :: Int -> Integer
fibTail n = fibHelper n 0 1

-- sinHelper

-- sinTail :: Int -> Int
-- sinTail x n = sinHelper


powHelper :: Int -> Int -> Int -> Integer
powHelper n c x
    | n == 0    = toInteger (c)
    | otherwise = powHelper (n - 1) ((c) * (x)) (x)

powX :: Int -> Int -> Integer
powX x n = powHelper n 1 x

sinusHelper :: Int->Int->Double->Double->Double->Double
sinusHelper n stepn x stepx current 
    | n + 1 == stepn = current
    | otherwise      =  sinusHelper n (stepn + 1) x ((stepx * (-1.0) * x * x) / (fromIntegral ((2 * (stepn + 1)) * (2 * (stepn + 1) + 1)) :: Double)) (stepx + current)

sinusX :: Double->Int->Double
sinusX x n = sinusHelper n 1 x ( (x * x  * x) /(-6.0)) x


lengthX :: [a] -> Int
lengthX [] = 0
lengthX (_:xs) = 1 + lengthX xs

atXHelper :: [a] -> Int -> Int -> a
atXHelper (element:xs) n current
    | current == n  = element
    | otherwise = atXHelper xs n (current + 1)

atX :: [a] -> Int -> a
atX l n = atXHelper l n 0

reverseXHelper :: [a] -> [a] -> [a]
reverseXHelper [] ys = ys
reverseXHelper (x:xs) ys = reverseXHelper xs (x:ys)

reverseX :: [a] -> [a]
reverseX xs = reverseXHelper xs []

-- concatXHelper :: [a] -> [a] -> [a] -> [a]
-- concatXHelper [] _ ys = ys

-- concatX :: [a] -> [a] -> [a]
-- concatX xs ys = concatXHelper xs ys []
 
-- concatXx :: [[a]] -> [a]



-- fmapX :: (a -> b) -> [a] -> [b]

fmapX :: (a -> b) -> [a] -> [b]  
fmapX _ [] = []  
fmapX f (x:xs) = f x : fmapX f xs





flatten :: [[a]] -> [a]
flatten [] = []
flatten ([]:vs) = flatten vs
flatten ((x:xs):vs) = x : flatten (xs:vs)



boolean :: [a] -> [[a]]
boolean [] = [[]]
boolean (x:xs) = let ys = boolean xs
                in ys ++ (map(x:) ys)


filterX :: (a -> Bool) -> [a] -> [a]   
filterX  _ [] = []
filterX f (x:xs) = if f x 
    then x : filterX f xs
    else filterX f xs