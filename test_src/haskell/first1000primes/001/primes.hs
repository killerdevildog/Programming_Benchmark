isPrime :: Int -> Bool
isPrime n
    | n < 2 = False
    | n == 2 = True
    | n `mod` 2 == 0 = False
    | otherwise = not $ any (\i -> n `mod` i == 0) [3,5..isqrt n]
    where isqrt = floor . sqrt . fromIntegral

main :: IO ()
main = print $ last $ take 1000 $ filter isPrime [2..]
