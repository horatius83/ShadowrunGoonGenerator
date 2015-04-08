module Goon (getGoon) where

import Cyberware (Cyberware, BodyPart)
import Hacking (Program)
import Stats (Stats(..), getStat)
import Weapons (Weapon, getWeapon, showWeapon)
import Skills (Skill, createSkill, SkillLevel(..), createSkillLevel)
import Spells (Spell)
import Armor (getArmor)
import Equipment (Equipment, getEquipment)

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
createGoonStats b a r s c i l w init = Stats b a r s c i l w 1 6.0 Nothing init 1 Nothing 10

createMagicGoonStats :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Stats
createMagicGoonStats b a r s c i l w m init = Stats b a r s c i l w 1 6.0 (Just m) init 1 Nothing 10

createGoon :: String -> Int -> Stats -> [(String, Int)] -> [String] -> [String] -> [String] -> Maybe [Cyberware] -> Maybe [Program] -> Maybe [Spell] -> Goon
createGoon name rank stats skills weapons armor equipment cyberware programs spells = Goon name rank stats skills' weapons' equipment' cyberware programs spells
    where
        skills' = [createSkill x (SkillLevel y) | (x,y) <- skills]
        weapons' = [getWeapon x | x <- weapons]
        equipment' = [getArmor x | x <- armor] ++ [getEquipment x | x <- equipment]

getGoon :: String -> Goon
getGoon name = undefined

rentacop = createGoon "Corporate Security Unit" 2 stats skills weapons armor equipment Nothing Nothing Nothing 
    where
        stats = createGoonStats 3 3 4 3 3 3 2 3 7 
        skills = [("Athletics", 3), ("Automatics", 3), ("Dodge", 3), ("Pistols", 3), ("Close Combat", 3)]
        weapons = ["Fischetti Security 600 Light Pistol", "HK-227X", "Stun Baton"]
        armor = ["Armor Vest"]
        equipment = ["CMT Clip"]


{--
stooby = Goon "Stooby" (createGoonStats 4 5 4 6 2 3 3 4 1) 
    [createSkill x y | (x,y) <- [("Clubs", 7), ("Dodge", 5), ("Intimidation", 5), ("Pistols", 7), ("Unarmed Combat", 7)]]
    [getWeapon "Ares Predator IV"]
    [getArmor "Armor Jacket"]
    (Just [Cyberware RightArm (createGoonStats 0 0 0 2 0 0 0 0 0)])
    Nothing
    Nothing

crank = Goon "Crank" (createGoonStats 5 3 3 6 2 2 2 3 1)
    [createSkill x y | (x,y) <- [("Automatics", 5), ("Blades", 6), ("Dodge", 4), ("Intimidation", 5), ("Unarmed Combat", 6)]]
    [getWeapon "AK-97 Carbine"]
    [getArmor "Armor Vest"]
    Nothing
    Nothing
    Nothing

joeby = Goon "Joeby" (createGoonStats 3 4 5 3 3 4 4 4 1)
    [createSkill x y | (x,y) <- [("Dodge", 7), ("Hacking", 7), ("Perception", 7), ("Pistols", 6), ("Unarmed Combat", 6)]]
    [getWeapon "Ares Predator IV"]
    [(getArmor "Armor Vest"), (Equipment "Novatech Airware Commlink w/Mangadyne Deva Os")]
    Nothing
    (Just [(Program "Attack" 4), (Program "Exploit" 4)]) 
    Nothing

fornis = Goon "Fornis" (createMagicGoonStats 3 3 3 3 3 3 5 5 4 1)
    [createSkill x y | (x,y) <- [("Counterspelling", 6), ("Perception", 5), ("Pistols", 5), ("Spellcasting", 7)]]
    [getWeapon "Fischetti Security 600 Light Pistol"]
    [getArmor "Clothing"]
    Nothing
    Nothing
    (Just [getSpell x | x <- ["Armor", "Confusion", "Ice Sheet", "Manabolt"]])

goons = [stooby, crank, joeby, fornis]
--}
printGoon :: Goon -> IO ()
printGoon goon = do
    putStrLn $ "Name: " ++ (goonName goon)
    putStrLn "Stats:"
    let statGetters = [getStat x | x <- [[y] | y <- "barscilw"] ++ ["ip"]]
        stats = [(x, getter $ goonStats goon) | (x, getter) <- statGetters]
        printListMaybe preface (Just x) = do
            putStrLn preface
            putStrLn . show $ x
        printListMaybe preface (Nothing) = do
            return ()
    mapM_ putStrLn [x ++ ": " ++ show y | (x,y) <- stats]
    putStrLn "Weapons: "
    mapM_ putStrLn [showWeapon (goonStats goon) weapon | weapon <- (goonWeapons goon)]
    putStrLn "Equipment:"
    mapM_ putStrLn [show equipment | equipment <- (goonEquipment goon)]
    printListMaybe "Cyberware: " (goonCyberware goon) 
    printListMaybe "Programs: " (goonPrograms goon)
    printListMaybe "Spells: " (goonSpells goon) 


