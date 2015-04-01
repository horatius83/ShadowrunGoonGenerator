module Hacking (
    Program,
    createDevice) where

data Program = Program { programName :: String, programLevel :: Int} deriving (Show)

data MatrixDevice = MatrixDevice { 
    name :: String,
    firewall :: Int, 
    response :: Int, 
    signal :: Int, 
    system :: Int,
    rating :: Int } deriving (Show)

createDevice :: String -> Int -> Int -> Int -> Int -> Int -> MatrixDevice
createDevice name firewall response signal system rating = MatrixDevice name firewall response signal' system rating'
    where
        inRange varName value minimum maximum 
            | value < minimum = error $ varName ++ " can not be less than " ++ (show minimum)
            | value > maximum = error $ varName ++ " can not be greater than " ++ (show maximum)
            | otherwise = value
        signal' = inRange "Signal" signal 0 9
        rating' = inRange "Rating" rating 1 6
