module Stats( Stats (Stats), getStat, getSpecialStat, statStrength ) where

import Data.List

data Stats = Stats {
    statBody :: Int, 
    statAgility :: Int, 
    statReaction :: Int, 
    statStrength :: Int, 
    statCharisma :: Int, 
    statIntuition :: Int, 
    statLogic :: Int, 
    statWillpower :: Int, 
    statEdge :: Int,
    statEssense :: Double, 
    statMagic :: Maybe Int,
    statInitiative :: Int,
    statInitiativePasses :: Int,
    statResonance :: Maybe Int,
    statConditionMonitor :: Int} deriving (Show)

stringifyStats :: (Show a) => [a] -> String
stringifyStats lst = (foldl (++) "" $ intersperse " " $ [show x | x <- lst]) ++ "\n"

getStat :: String -> (String, (Stats -> Int))
getStat name = case name of
    "b" -> ("Body", statBody)
    "a" -> ("Agility", statAgility)
    "r" -> ("Reaction", statReaction)
    "s" -> ("Strength", statStrength)
    "c" -> ("Charisma", statCharisma)
    "i" -> ("Intuition", statIntuition)
    "l" -> ("Logic", statLogic)
    "w" -> ("Willpower", statWillpower)
    "ip" -> ("Initiative Passes", statInitiativePasses)
    _ -> ("Invalid Stat: " ++ name, \_ -> 0)

getSpecialStat :: String -> (String, (Stats -> Maybe Int))
getSpecialStat name = case name of
    "m" -> ("Magic", statMagic)
    "res" -> ("Resonance", statResonance)
    _ -> ("Invalid Stat: " ++ name, \_ -> Nothing) 
