module Cyberware (Cyberware(..), BodyPart(..)) where

import Stats (Stats)

data BodyPart = RightArm | LeftArm | RightLeg | LeftLeg deriving (Show)
data Cyberware = Cyberware BodyPart Stats deriving (Show)
