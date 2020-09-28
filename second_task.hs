primes = sieve [2..]
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]

foldM:: (a -> b -> b) -> b -> [a] -> b
foldM _ b [] = b
foldM f b (x:xs) = foldM f (f x b) xs 

unfoldM :: (s -> Maybe (a, s)) -> s -> [a]
unfoldM f state = case f state of
  Just (x, newState) -> x : unfoldM f newState
  Nothing            -> []