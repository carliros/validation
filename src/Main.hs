module Main where

main :: IO ()
main = do
  putStrLn "hello world"

-- Ex. 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits . show

toDigits :: String -> [Integer]
toDigits n = map toRead n
  where toRead :: Char -> Integer
        toRead n = read (n : [])


-- Ex. 2
doubleSecond :: (Num a) => [a] -> [a]
doubleSecond list = let zipList = zip list [1 .. (length list)]
                        multPar (n, p) = if ( p `mod` 2 == 0) then n * 2 else n
                    in map multPar zipList

-- Ex. 3
sumDigits :: [Integer] -> Integer
sumDigits = undefined

-- Ex. 4
validate :: Integer -> Bool
validate = undefined

-- Ex. 5
readCC :: String -> Integer
readCC = undefined

-- Ex. 6
showCC :: Integer -> String
showCC = undefined

-- Ex. 7
lookupIssuer :: String -> Integer -> IO String
lookupIssuer = undefined

-- Ex. 8
checkCC :: String -> IO ()
checkCC = undefined

-- Ex. 9: BONUS! Leave undefined if you do not do it.
toDigitsRevG :: (Integral a) => a -> a -> [a]
toDigitsRevG = undefined
