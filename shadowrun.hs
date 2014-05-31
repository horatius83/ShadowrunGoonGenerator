import Data.Maybe
import Data.Map
import Prelude hiding (lookup)
import Weapons
import Stats
import Armor
import Equipment
import Skills

createGoonStats b a r s c i l w ip = Stats b a r s c i l w 1 6.0 Nothing 1 ip Nothing 10
createMagicGoonStats b a r s c i l w m ip = Stats b a r s c i l w 1 6.0 (Just m) 1 ip Nothing 10

data Movement = Move { moveWalking :: Int, moveSprinting :: Int} deriving (Show)

data Power = Passive String | Active String Damage deriving (Show)

data Program = Program { programName :: String, programLevel :: Int} deriving (Show)

data BodyPart = RightArm | LeftArm | RightLeg | LeftLeg deriving (Show)
data Cyberware = Cyberware BodyPart Stats deriving (Show)

data SpellType = Physical | Mana deriving (Show)
data SpellRange = LoS | LoSArea deriving (Show)
data SpellDuration = Sustained | Instant deriving (Show)
data Spell = Spell {
    spellName :: String, 
    spellType :: SpellType,
    spellRange :: SpellRange,
    spellDuration :: SpellDuration,
    spellDescription :: String } deriving (Show)

notFoundSpell x = Spell ("Could not find '" ++ x ++ "' in spell database") Mana LoS Instant ""
getSpell x = findWithDefault (notFoundSpell x) x spellDb
spellDb = fromList [(spellName x, x) | x <- [
    Spell "Armor" Physical LoS Sustained "Both ballistic and Impact protection equal to hits scored, cumulative with worn armor.",
    Spell "Confusion" Mana LoS Sustained "-1 dice pool modifier to target per hit",
    Spell "Ice Sheet" Physical LoSArea Instant "Crossing ice requires Agility + Reaction Test, Threshold equal to hits, to avoid falling",
    Spell "Manabolt" Mana LoS Instant "Damage: (equal to hits)P"]]

type Weaknesses = [String]

data Goon = Goon {
    goonName :: String,
    goonStats :: Stats,
    goonSkills :: [Skill],
    goonWeapons :: [Weapon],
    goonEquipment :: [Equipment],
    goonCyberware :: Maybe [Cyberware],
    goonPrograms :: Maybe [Program],
    goonSpells :: Maybe [Spell]} deriving (Show)

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
    --mapM_ putStrLn [printWeapon weapon (goonStats goon) | weapon <- (goonWeapons goon)]
    mapM_ putStrLn [showWeapon (goonStats goon) weapon | weapon <- (goonWeapons goon)]
    putStrLn "Equipment:"
    mapM_ putStrLn [show equipment | equipment <- (goonEquipment goon)]
    printListMaybe "Cyberware: " (goonCyberware goon) 
    printListMaybe "Programs: " (goonPrograms goon)
    printListMaybe "Spells: " (goonSpells goon) 

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
createCreatureStats b a r s c i l w edge essence initiative ip = Stats b a r s c i l w edge essence Nothing initiative ip Nothing 10

data Creature = Creature { 
    creatureName :: String, 
    creatureStats :: Stats, 
    creatureMovement :: Movement, 
    creatureSkills :: [Skill], 
    creaturePowers :: Maybe [Power], 
    creatureWeaknesses :: Maybe Weaknesses, 
    creatureNotes :: Maybe String} deriving (Show)

dog = Creature "Dog" (createCreatureStats 2 3 3 2 3 3 1 3 3 6 6 1) 
    (Move 10 45) 
    [createSkill x 2 | x <- ["Intimidation", "Perception", "Tracking", "Unarmed Combat"]]
    (Just [(Passive "Enhanced Senses (Smell)"), (Active "Natural Weapon" (dmg 5 0))]) 
    Nothing 
    Nothing

greatCat = Creature "Great Cat" (createCreatureStats 6 5 4 5 3 3 2 3 4 6 7 2) 
    (Move 10 60) 
    [createSkill x y | (x,y) <- [("Infiltration", 3), ("Perception", 2), ("Tracking", 3), ("Unarmed Combat", 4)]]
    (Just [(Active "Natural Weapon" (dmg 5 0))]) 
    Nothing 
    Nothing

horse = Creature "Horse" (createCreatureStats 8 5 5 8 3 3 1 2 2 6 8 1) 
    (Move 20 100) 
    [createSkill "Running" 3]
    Nothing 
    Nothing 
    Nothing

shark = Creature "Shark" (createCreatureStats 3 5 5 3 1 4 1 2 2 6 9 1) 
    (Move 20 60) 
    [createSkill x y | (x,y) <- [("Perception", 2), ("Swimming", 4), ("Unarmed Combat", 4)]]
    (Just [(Active "Natural Weapon" (dmg 5 0))]) 
    Nothing 
    Nothing

wolf = Creature "Wolf" (createCreatureStats 2 3 3 2 3 3 2 3 3 6 6 2) 
    (Move 10 50) 
    [createSkill x y | (x,y) <- [("Infiltration", 2), ("Perception", 2), ("Tracking", 2), ("Unarmed Combat", 4)]]
    (Just [Active "Natural Weapon" (dmg 2 0)]) 
    Nothing 
    Nothing
