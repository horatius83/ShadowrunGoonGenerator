module Cyberware (Cyberware(..), BodyPart(..), CyberLimbEnhancement(..)) where

import Weapons (Weapon(..), WeaponType(..), AP(..), DamageType(..), Damage(..))
import Data.Map (Map, lookup, fromList)
import Data.List (foldl')
import Prelude hiding (lookup)

data BodyPart = Arm | Leg | Torso | Hand | Foot | Forearm | LowerLeg | Head | Eyes | Ears deriving (Show)
data CyberLimbEnhancement = Armor Int | Body Int | Strength Int | Agility Int deriving (Show)
data CyberWeaponAccessory = CyberWeaponAccessory String Double Int deriving (Show)
data CyberLimbAccessory = CyberLimbAccessory String Int BodyPart | CyberWeapon Double Int Weapon [CyberWeaponAccessory] BodyPart deriving (Show)
data Cyberware = CyberLimb String BodyPart (Maybe CyberLimbEnhancement) Double Int [CyberLimbAccessory] | Cyberware String BodyPart Double Int deriving (Show)

getCyberware :: String -> Maybe Cyberware
getCyberware name = lookup name cyberwareDb

getName :: Cyberware -> String
getName (CyberLimb name _ _ _ _ _) = name
getName (Cyberware name _ _ _ ) = name 

cyberwareDb :: Map String Cyberware
cyberwareDb = fromList $ cyberwareList ++ cyberlimbList 
    where
        createCyberwareList tuples bodyPart = [(name, Cyberware name bodyPart essense capacity) | (name, essense, capacity) <- tuples]
        cyberwareTuples = [(Head, headware), (Eyes, eyeware), (Ears, earware), (Torso, bodyware)]
        cyberwareList = foldl' (++) [] $ [createCyberwareList tuples part | (part, tuples) <- cyberwareTuples] 
        cyberlimbList = [(name, CyberLimb name part Nothing essense capacity []) | (name, essense, capacity, part) <- cyberlimbs]  

headware :: [(String, Double, Int)]
headware = [
    ("Commlink", 0.2, 2),
    ("Control Rig", 0.5, 0),
    ("Cranial Kink Bomb", 0.0, 1),
    ("Cranial Micro Bomb", 0.0, 2),
    ("Cranial Area Bomb", 0.0, 3),
    ("Datajack", 0.1, 1),
    ("Data Lock", 0.1, 1),
    ("Sim Module", 0.2, 2),
    ("Hot-Sim Module", 0.2, 2),
    ("Tooth Storage Compartment", 0.0, 0),
    ("Tooth Breakable Compartment", 0.0, 0),
    ("Ultrasound Sensor", 0.3, 2),
    ("Voice Modulator", 0.2, 0)] 
        ++ [("Olfactory Booster " ++ (show i), 0.2, 2) | i <- [1..6] :: [Int]] 
        ++ [("Taste Booster " ++ (show i), 0.2, 0) |  i <- [1..6] :: [Int]]
        ++ [("Voice Modulator w/ Secondary Pattern " ++ (show i), 0.2, 0) | i <- [1..6] :: [Int]]

eyeware :: [(String, Double, Int)]
eyeware = [
    ("Eye Recording Unit", 0.1, 0),
    ("Flare Compensation", 0.1, 1),
    ("Image Link", 0.1, 4),
    ("Low-Light Vision", 0.1, 2),
    ("Ocular Drone", 0.0, 6),
    ("Protective Covers", 0, 0),
    ("Smartlink", 0.1, 3),
    ("Thermographic Vision", 0.1, 2),
    ("Vision Magnification", 0.1, 2)]
        ++ [("Cybereyes " ++ (show i), 0.1 + (fromIntegral i) * 0.1, i * 4) | i <- [1..4] :: [Int]] 
        ++ [("Retinal Duplication " ++ (show i), 0.1, 1) | i <- [1..6] :: [Int]]
        ++ [("Vision Enhancement " ++ (show i), 0.1, i) | i <- [1..3] :: [Int]]

earware :: [(String, Double, Int)]
earware = [
    ("Balance Augmenter", 0.1, 4),
    ("Damper", 0.1, 1),
    ("Ear Recording Unit", 0.1, 0),
    ("Sound Link", 0.1, 0),
    ("Spatial Recognizer", 0.1, 2)]
        ++ [("Cyberears " ++ (show i), 0.1 + (fromIntegral i) * 0.1, 4 * i) | i <- [1..4] :: [Int]]
        ++ [("Audio Enhancement " ++ (show i), 0.1, i) | i <- [1..3] :: [Int]]
        ++ [("Select Sound Filter " ++ (show i), 0.1, i) | i <- [1..6] :: [Int]]

bodyware :: [(String, Double, Int)]
bodyware = [ 
    ("Plastic Bone Lacing", 0.5, 0),
    ("Aluminum Bone Lacing", 1.0, 0),
    ("Titanium Bone Lacing", 1.5, 0),
    ("Cosmetic Modification", 0.0, 0),
    ("Fingertip Compartment", 0.1, 1),
    ("Grapple Gun", 0.5, 5),
    ("Internal Air Tank", 0.25, 3),
    ("Simrig", 0.5, 0),
    ("Smuggling Compartment", 0.2, 2),
    ("Touch Link", 0.1, 0)]
        ++ [("Dermal Plating " ++ (show i), 0.5 * (fromIntegral i), 0) | i <- [1..3] :: [Int]]
        ++ [("Muscle Replacement " ++ (show i), fromIntegral i, 0) | i <- [1..4] :: [Int]]
        ++ [("Reaction Enhancers " ++ (show i), 0.3 * fromIntegral i, 0) | i <- [1..3] :: [Int]]
        ++ [("Skillwires " ++ (show i), 0.2 * fromIntegral i, 0) | i <- [1..5] :: [Int]]
        ++ [("Wired Reflexes " ++ (show i), j, 0) | (i,j) <- [(1, 2.0), (2, 3.0), (3, 5.0)] :: [(Int, Double)]] 

cyberlimbs :: [(String, Double, Int, BodyPart)]
cyberlimbs = [("Obvious " ++ (show i), j, k, l) | (i,j,k,l) <- [
    ("Full Arm", 1.0, 15, Arm),
    ("Full Leg", 1.0, 20, Leg),
    ("Hand", 0.25, 4, Hand),
    ("Foot", 0.25, 4, Foot),
    ("Lower Arm", 0.45, 10, Arm),
    ("Lower Leg", 0.45, 12, Leg),
    ("Torso", 1.5, 10, Torso),
    ("Skull", 0.75, 4, Head)]]
        ++ [("Synthetic " ++ (show i), j, k, l) | (i,j,k,l) <- [
                ("Full Arm", 1.0, 8, Arm),
                ("Full Leg", 1.0, 10, Leg),
                ("Hand", 0.25, 2, Hand),
                ("Foot", 0.25, 2, Foot),
                ("Lower Arm", 0.45, 5, Arm),
                ("Lower Leg", 0.45, 6, Leg),
                ("Torso", 1.5, 5, Torso)]]

cyberLimbAccessories :: Map String CyberLimbAccessory
cyberLimbAccessories = fromList [(name, convertToCyberAcc name capacity part) | (name, capacity, part) <- accs] 
    where
        convertToCyberAcc name capacity part = CyberLimbAccessory name capacity part
        accs = [
            ("Cyberarm Gyromount", 4, Arm),
            ("Cyberarm Slide", 8, Arm)]
                ++ [("Large Smuggling Compartment in " ++ i, 5, l) | (i,l) <- [("Arm", Arm), ("Leg", Leg)]]
                ++ [("Cyber Holster in " ++ i, 7, j) | (i,j) <- [("Arm", Arm), ("Leg", Leg), ("Torso", Torso)]]
                ++ [("Hydraulic Jacks " ++ (show i), i, Leg) | i <- [1..6]]

createCyberWeapon :: Weapon -> [CyberWeaponAccessory] -> BodyPart -> Maybe CyberLimbAccessory
createCyberWeapon weapon accessories part = 
    case weaponType weapon of
        HoldOut -> cw 0.15 2
        LightPistol -> cw 0.35 4
        MachinePistol -> cw 0.4 4
        HeavyPistol -> cw 0.6 6
        SubMachineGun -> cw 1.0 10
        Shotgun -> cw 1.1 11
        GrenadeLauncher -> cw 1.5 15
        _ -> Nothing
        where
            cw essense capacity = Just $ CyberWeapon essense capacity weapon accessories part

createCyberMeleeWeapon :: String -> Maybe CyberLimbAccessory
createCyberMeleeWeapon name = 
    case name of
        "Hand Blade" -> Just $ CyberWeapon 0.25 3 (Weapon "Hand Blade" (Dmg (DmgPhysical (\x -> 2 + x `div` 2))  (AP 0)) Blade Nothing Nothing Nothing Nothing Nothing) [] Hand
        "Hand Razors" -> Just $ CyberWeapon 0.2 2 (Weapon "Hand Razors" (Dmg (DmgPhysical (\x -> 1 + x `div` 2)) (AP 0)) Blade Nothing Nothing Nothing Nothing Nothing) [] Hand
        "Spur" -> Just $ CyberWeapon 0.3 3 (Weapon "Spur" (Dmg (DmgPhysical (\x -> 3 + x `div` 2)) (AP 0)) Blade Nothing Nothing Nothing Nothing Nothing) [] Hand
        "Shock Hand" -> Just $ CyberWeapon 0.25 3 (Weapon "Shock Hand" (Dmg (DmgStun (\x -> 6)) Half) Unarmed Nothing Nothing Nothing (Just 10) Nothing) [] Hand
        _ -> Nothing
        
cyberWeaponAccessories :: Map String CyberWeaponAccessory
cyberWeaponAccessories = fromList [(name, CyberWeaponAccessory name essense capacity) | (name, essense, capacity) <- accessories]
    where
        accessories = [
            ("External Clip Port", 0.1, 1),
            ("Laser Sight", 0.1, 1),
            ("Silencer", 0.2, 2),
            ("Sound Suppressor", 0.3, 3)]
