module Main where

import           Control.Monad.Trans
import           System.Console.Haskeline

main :: IO ()
main = checkCC "data.txt"

-- Ex. 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

toDigits :: Integer -> [Integer]
toDigits n = map toRead (show n)
  where toRead :: Char -> Integer
        toRead n = read (n : [])


-- Ex. 2
doubleSecond :: (Num a) => [a] -> [a]
doubleSecond list = let zipList = zip list [1 .. (length list)]
                        multPar (n, p) = if ( p `mod` 2 == 0) then n * 2 else n
                    in map multPar zipList

-- Ex. 3
sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits

-- Ex. 4
validate :: Integer -> Bool
validate n = let rev = toDigitsRev n
                 dob = doubleSecond rev
                 nn = sumDigits dob
             in (nn `mod` 10 == 0)

-- Ex. 5
readCC :: String -> Integer
readCC = read . concat . words

-- Ex. 6
showCC :: Integer -> String
showCC = show

-- Ex. 7
lookupIssuer :: String -> Integer -> IO String
lookupIssuer file cc
  = do content <- readFile file
       let issuers = buildIssuerList content
       case searchIssuer issuers (show cc) of
        Just issuer -> return issuer
        Nothing     -> return "Unknown"

searchIssuer :: [(String, Int, String)] -> String -> Maybe String
searchIssuer [] _ = Nothing
searchIssuer ((prefix, len, issuer) : ls) cc = if compare then Just issuer else searchIssuer ls cc
  where compare = let ccLen = take (length prefix) cc
                  in prefix == ccLen && (length cc) == len

buildIssuerList :: String -> [(String, Int, String)]
buildIssuerList content
  = let listLine = lines content
    in concat $ map (toTuple . words) listLine
    where toTuple :: [String] -> [(String, Int, String)]
          toTuple [] = []
          toTuple (prefix : len : issuer) = [(prefix, read len, concat issuer)]


-- Ex. 8
checkCC :: String -> IO ()
checkCC file = runInputT defaultSettings loop
  where loop :: InputT IO ()
        loop = do
             minput <- getInputLine "Enter credit card number: "
             case minput of
                 Nothing -> return ()
                 Just input -> do liftIO $ processInput input
                                  loop
        processInput :: String -> IO ()
        processInput input = do let cc = readCC input
                                if validate cc
                                  then do issuer <- lookupIssuer file cc
                                          putStrLn $ "The number " ++ input ++ " is valid and the type is " ++ issuer ++ "."
                                  else putStrLn $ "The number " ++ input ++ " is not a valid credit card number."


-- Ex. 9: BONUS! Leave undefined if you do not do it.
toDigitsRevG :: (Integral a) => a -> a -> [a]
toDigitsRevG = undefined
