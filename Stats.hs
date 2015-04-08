module Stats( Stats (Stats), getStat, getSpecialStat, statStrength, Initiative) where

import Data.List
import Data.Map (Map, fromList)
import qualified Data.Set as S

type Stats = Map String Double

createStats :: [(String, Double)] -> Stats
createStats stats
    | all (\(s,_) -> S.member s statNames) stats = fromList stats
    | otherwise = error $ "The following values are not stats: " ++ unknownStats
    where
        unknownStatsString = foldl' (++) "" $ intersperse ", " unknownStats
        unknownStats = map fst $ filter (\(s,_) -> not $ S.member s statNames) stats 

statNames :: S.Set String
statNames = S.fromList ["body", "agility", "reaction", "strength", "charisma", "intuition", "logic", "willpower",
    "edge", "essense", "magic", "initiative", "initiative passes", "resonance", "condition monitor", 
    "astral initiative", "matrix initiative"]

{--data Stats = Stats {
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
--}
