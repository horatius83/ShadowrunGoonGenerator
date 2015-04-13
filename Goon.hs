module Goon (getGoon) where

import Data.Map (insert)

import Cyberware (Cyberware(..), BodyPart(..))
import Hacking (Program)
import Stats (Stats, createStatsShort, getStat)
import Weapons (Weapon, getWeapon, showWeapon)
import Skills (Skill, createSkill, SkillLevel(..), createSkillLevel)
import Spells (Spell, getSpell)
import Armor (getArmor)
import Equipment (Equipment(..), getEquipment, FocusType(..))

data Goon = Goon {
    goonName :: String,
    goonRating :: Int,
    goonStats :: Stats,
    goonSkills :: [Skill],
    goonWeapons :: [Weapon],
    goonEquipment :: [Equipment],
    goonCyberware :: Maybe [Cyberware],
    goonPrograms :: Maybe [Program],
    goonSpells :: Maybe [Spell]} deriving (Show)

createGoonStats :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Stats
createGoonStats b a r s c i l w init = createStatsShort $ createBaseStats b a r s c i l w 1 init 1 10 

createMagicGoonStats :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Double -> Int -> Int -> Int -> Int -> Int -> Int -> Stats
createMagicGoonStats b a r s c i l w ess m init ip cm astralInit astralIp = createStatsShort $ statTuples ++ magicalStatTuples 
    where
        magicalStatTuples = zip ["m", "astral init", "astral ip"] $ map fromIntegral [m, astralInit, astralIp] 
        statTuples = createBaseStats b a r s c i l w ess init ip cm 

createGoon :: String -> Int -> Stats -> [(String, Int)] -> [String] -> [String] -> [String] -> Maybe [Cyberware] -> Maybe [Program] -> Maybe [Spell] -> Goon
createGoon name rank stats skills weapons armor equipment cyberware programs spells = Goon name rank stats skills' weapons' equipment' cyberware programs spells
    where
        skills' = [createSkill x (SkillLevel y) | (x,y) <- skills]
        weapons' = [getWeapon x | x <- weapons]
        equipment' = [getArmor x | x <- armor] ++ [getEquipment x | x <- equipment]

createSkillList :: [(String, Int)] -> [Skill]
createSkillList skills = [createSkill x (SkillLevel y) | (x,y) <- skills]

createBaseStats :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Double -> Int -> Int -> Int -> [(String,Double)]
createBaseStats b a r s c i l w ess init ip cm = statsTuples ++ [("ess", ess)] 
    where
        statsTuples = zip ["b", "a", "r", "s", "c", "i", "l", "w", "e", "init", "ip", "cm"] statsList 
        statsList = map fromIntegral [b,a,r,s,c,i,l,w,1,init,ip,cm] 

getGoon :: String -> Goon
getGoon name = undefined

rentacop = createGoon "Corporate Security Unit" 2 stats skills weapons armor equipment Nothing Nothing Nothing 
    where
        stats = createGoonStats 3 3 4 3 3 3 2 3 7 
        skills = [("Athletics", 3), ("Automatics", 3), ("Dodge", 3), ("Pistols", 3), ("Close Combat", 3)]
        weapons = ["Fischetti Security 600 Light Pistol", "HK-227X", "Stun Baton"]
        armor = ["Armor Vest"]
        equipment = ["CMT Clip"]

rentaCopLt :: Goon
rentaCopLt = Goon "CorpSec Lieutenant" 2 stats skills weapons equipment (Just cyberware) Nothing (Just spells)
    where
        stats = createMagicGoonStats 3 3 3 3 3 4 3 4 6.0 3 7 1 10 8 3
        skills = createSkillList [("Assensing", 3), ("Astral Combat", 4), ("Conjuring", 3), ("Leadership", 2), ("Pistols", 2), ("Sorcery", 4)]
        weapons = [getWeapon "Fischetti Security 600 Light Pistol"]
        equipment = [getArmor "Armor Vest", getEquipment "Renraku Sensei", Focus Spellcasting 2] 
        cyberware = [Cyberware RightArm cyberArmStats]
        cyberArmStats = createStatsShort $ createBaseStats 0 0 0 0 0 0 0 0 0 0 0 1
        spells = map getSpell ["Detect Life", "Light", "Physical Barrier", "Powerbolt", "Silence", "Stunball"]        

{--rentaCopPrime = createGoon "CorpSec Lieutenant" 2 stats skills weapons armor equipment Nothing Nothing (Just spells)
    where
        baseStats = createMagicGoonStats 3 3 3 3 3 4 3 4 3 7
        stats = insert "astral initiative" 8 $ insert "astral initiative passes" 3 baseStats
        skills = [("Assensing",3), ("Astral Combat", 4), ("Conjuring", 3), ("Leadership", 2), ("Pistols", 2), ("Sorcery", 4)]
        weapons = ["Fischetti Security 600 Light Pistol"]
        armor = ["Armor Vest"]
        equipment = ["Renraku Sensei"]
        spells = [getSpell x | x <- ["Detect Life", "Light", "Physical Barrier", "Powerbolt", "Silence", "Stunball"]]
-- Need a way to add spell focus to equipment
--}

printGoon :: Goon -> IO ()
printGoon goon = do
    putStrLn $ "Name: " ++ (goonName goon)
    putStrLn "Stats:"
     


