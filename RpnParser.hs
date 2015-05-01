module RpnParser (parseRpn, parseRpnFromList) where

import Data.List (foldl')

parseRpn :: String ->  Double
parseRpn expression = parseRpnFromList tokens
    where
        tokens = words expression
        
parseRpnFromList :: [String] -> Double
parseRpnFromList tokens = head $ foldl' parse [] tokens
    where
        parse (x:y:xs) "+" = x + y : xs
        parse (x:y:xs) "-" = y - x : xs
        parse (x:y:xs) "/" = y / x : xs
        parse (x:y:xs) "*" = x * y : xs
        parse xs x = (read x):xs 

