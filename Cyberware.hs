module Cyberware (Cyberware) where

data BodyPart = RightArm | LeftArm | RightLeg | LeftLeg deriving (Show)
data Cyberware = Cyberware BodyPart Stats deriving (Show)
