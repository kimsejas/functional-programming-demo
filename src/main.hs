-- Write printAMessage here
printAMessage :: String -> IO ()
printAMessage x = putStrLn x

-- Write division here
division :: Double -> Double -> Maybe Double
division x y
  | y == 0    = Nothing
  | otherwise = Just (x / y)

-- Write factorial here
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

-- Write factList here
factList :: Int -> [Int]
factList n = map factorial [1..n]
  where
    factorial :: Int -> Int
    factorial 0 = 1
    factorial 1 = 1
    factorial x = x * factorial (x - 1)

-- Write merge here
merge :: [Int] -> [Int] -> [Int]
merge [] ls2 = ls2
merge ls1 [] = ls1
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

main :: IO ()
main = do
    print (merge [1, 3, 5] [2, 4, 6]) -- Expected: [1, 2, 3, 4, 5, 6]