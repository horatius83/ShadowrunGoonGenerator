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

stringifyStats lst = (foldl (++) "" $ intersperse " " $ [show x | x <- lst]) ++ "\n"

{--instance Show Stats where
    show (Stats b a r c i l w e ess Nothing init ip Nothing) = show "b a r c i l w e ess init ip\n" ++ stringifyStats [b, a, r, c, i, l, w, e, ess, init, ip] 
    show (Stats b a r c i l w e ess (Just m) init ip Nothing) = show "b a r c i l w e ess m*    
--}
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
    otherwise -> ("Invalid Stat: " ++ name, \x -> 0)

getSpecialStat name = case name of
    "m" -> ("Magic", statMagic)
    "res" -> ("Resonance", statResonance)
