module Cyberware (Cyberware(..), BodyPart(..), CyberLimbEnhancement(..)) where

data BodyPart = Arm | Leg | Torso | Hand | Foot | Forearm | LowerLeg | Head | Eyes | Ears deriving (Show)
data CyberLimbEnhancement = Armor Int | Body Int | Strength Int | Agility Int deriving (Show)
data Cyberware = CyberLimb BodyPart CyberLimbEnhancement Double Int | Cyberware BodyPart Double Int deriving (Show)

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

cyberlimbs :: [(String, Double, Int)]
cyberlimbs = [("Obvious " ++ (show i), j, k) | (i,j,k) <- [
    ("Full Arm", 1.0, 15),
    ("Full Leg", 1.0, 20),
    ("Hand", 0.25, 4),
    ("Foot", 0.25, 4),
    ("Lower Arm", 0.45, 10),
    ("Lower Leg", 0.45, 12),
    ("Torso", 1.5, 10),
    ("Skull", 0.75, 4)]]
        ++ [("Synthetic " ++ (show i), j, k) | (i,j,k) <- [
                ("Full Arm", 1.0, 8),
                ("Full Leg", 1.0, 10),
                ("Hand", 0.25, 2),
                ("Foot", 0.25, 2),
                ("Lower Arm", 0.45, 5),
                ("Lower Leg", 0.45, 6),
                ("Torso", 1.5, 5)]]
