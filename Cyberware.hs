module Cyberware (Cyberware(..), BodyPart(..), CyberLimbEnhancement(..)) where

import Stats (Stats)

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
        ++ [("Olfactory Booster " ++ (show i), 0.2, 2) | i <- [1..6]] 
        ++ [("Taste Booster " ++ (show i), 0.2, 0) |  i <- [1..6]]
        ++ [("Voice Modulator w/ Secondary Pattern " ++ (show i), 0.2, 0) | i <- [1..6]]

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
        ++ [("Cybereyes " ++ (show i), 0.1 + (fromIntegral i) * 0.1, i * 4) | i <- [1..4]] 
        ++ [("Retinal Duplication " ++ (show i), 0.1, 1) | i <- [1..6]]
        ++ [("Vision Enhancement " ++ (show i), 0.1, i) | i <- [1..3]]

earware :: [(String, Double, Int)]
earware = [
    ("Balance Augmenter", 0.1, 4),
    ("Damper", 0.1, 1),
    ("Ear Recording Unit", 0.1, 0),
    ("Sound Link", 0.1, 0),
    ("Spatial Recognizer", 0.1, 2)]
        ++ [("Cyberears " ++ (show i), 0.1 + (fromIntegral i) * 0.1, 4 * i) | i <- [1..4]]
        ++ [("Audio Enhancement " ++ (show i), 0.1, i) | i <- [1..3]]
        ++ [("Select Sound Filter " ++ (show i), 0.1, i) | i <- [1..6]]

bodyware :: [(String, Double, Int)]
bodyware = undefined
