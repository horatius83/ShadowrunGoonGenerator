module RpnParser (parseRpn) where

import Data.Char (isAlphaNum)
import Data.List (foldl')

parseRpn :: String -> (String -> String) -> Double
parseRpn expression varToNumber = head $ foldl' parse [] tokens
    where
        tokens = map varToNumber $ words expression
        parse (x:y:xs) "+" = x + y : xs
        parse (x:y:xs) "-" = x - y : xs
        parse (x:y:xs) "/" = x / y : xs
        parse (x:y:xs) "*" = x * y : xs
        parse xs x = (read x):xs 
