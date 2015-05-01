module Weapons 
    (Magazine, 
    WeaponType(..), 
    Weapon(..),
    Damage (Dmg),
    AP(..),
    DamageType(..),
    weaponDb,
    getWeapon,
    dmg,
    getDamage,
    showWeapon 
) where

import Data.Map (lookup, member, findWithDefault, Map, fromList) 
import RpnParser (parseRpnFromList)
import Stats
import Data.List
import Data.Maybe (fromJust)

data AP = AP Int | Half deriving (Show)

data DamageType = DmgPhysical String | DmgStun String deriving (Show)
data Damage = Dmg DamageType AP | Grenade | Chemical | SpecialDmg AP | Toxin AP | Missile deriving (Show)

replaceStatsWithValues :: String -> Stats -> [String]
replaceStatsWithValues rpn stats = Data.List.map convertToken tokens
    where
        tokens = words rpn
        convertToken token
            | token `member` shortStatToLong = show $ fromJust $ Data.Map.lookup (longName token) stats
            | otherwise = token
        longName x = fromJust $ Data.Map.lookup x shortStatToLong
 

getDamage :: Stats -> Damage -> (Int, Int, AP) 
getDamage stats (Dmg (DmgPhysical rpn) ap) = (round $ parseRpnFromList $ replaceStatsWithValues rpn stats, 0, ap)
getDamage stats (Dmg (DmgStun rpn) ap) = (0, round $ parseRpnFromList $ replaceStatsWithValues rpn stats, ap)
getDamage _ _ = error "Can not calculate damage"
            
data Magazine = Magazine { shotsRemaining :: Int, shots :: Int} deriving (Show)

data WeaponMode = SingleShot | SemiAutomatic | BurstFire | Automatic deriving (Show)

data WeaponType = Blade | Club | Exotic | Unarmed | Bow | Crossbow | ThrowingWeapon 
    | Taser | HoldOut | LightPistol | HeavyPistol
    | MachinePistol | SubMachineGun | AssaultRifle | AssultRifle | SportRifle | SniperRifle | Shotgun | Special 
    | LightMachineGun | MediumMachineGun | HeavyMachineGun | AssaultCannon 
    | GrenadeLauncher | MissileLauncher deriving (Show)

data Weapon = Weapon { 
    weaponName :: String, 
    weaponDmg :: Damage, 
    weaponType :: WeaponType, 
    weaponReach :: Maybe Int,
    weaponRecoil :: Maybe Int,
    weaponModes :: Maybe [WeaponMode],
    weaponAmmoCapacity :: Maybe Int,
    weaponMinimumStrength :: Maybe Int} deriving (Show)

showWeapon :: Stats -> Weapon -> String
showWeapon stats wpn =
    let (physical, stun, ap) = getDamage stats (weaponDmg wpn)
        weaponStats = [weaponName wpn, " Physical: ", show physical, " Stun: ", show stun, " AP: ", show ap]
    in Data.List.foldl (++) "" weaponStats

ranged :: String -> Damage -> WeaponType -> [WeaponMode] -> Int -> Weapon
ranged name damage wpnType modes ammoCapacity = Weapon name damage wpnType Nothing Nothing (Just modes) (Just ammoCapacity) Nothing

rranged :: String -> Damage -> WeaponType -> [WeaponMode] -> Int -> Int -> Weapon
rranged name damage wpnType modes recoil ammoCapacity = Weapon name damage wpnType Nothing (Just recoil) (Just modes) (Just ammoCapacity) Nothing 

melee :: String -> Damage -> WeaponType -> Int -> Weapon
melee name damage wpnType reach = Weapon name damage wpnType (Just reach) Nothing Nothing Nothing Nothing 

dmg :: Int -> Int -> Damage
dmg hp ap = Dmg (DmgPhysical $ show hp) (AP ap)

physicalDmg :: String -> Int -> Damage
physicalDmg formula ap = Dmg (DmgPhysical formula) (AP ap) 

notFoundWeapon :: String -> Weapon
notFoundWeapon x = melee ("Could not find '" ++ x ++ "' in weapon database.") (Dmg (DmgPhysical "0") (AP 0)) Blade 0

getWeapon :: String -> Weapon
getWeapon x = findWithDefault (notFoundWeapon x) x weaponDb

weaponDb :: Map String Weapon
weaponDb = fromList [(weaponName x, x) | x <- weaponList ++ bowList ]

weaponList :: [Weapon]
weaponList = [
    -- Blades
    melee "Combat Axe" (physicalDmg "s 2 / 4 +" (-1)) Blade 2,
    melee "Forearm Snap Blades" (physicalDmg "s 2 / 2 +" 0) Blade 0,
    melee "Katana" (physicalDmg "s 2 / 3 +" (-1)) Blade 1,
    melee "Knife" (physicalDmg  "s 2 / 1 +" 0) Blade 0,
    melee "Monofilament Sword" (physicalDmg "s 2 / 3 +" (-1)) Blade 1,
    melee "Survival Knife" (physicalDmg "s 2 / 1 +" (-1)) Blade 0,
    melee "Sword" (physicalDmg "s 2 / 3 +" 0) Blade 1,
    --Clubs
    melee "Club" (physicalDmg "s 2 / 1 +" 0) Club 1,
    melee "Extendable Baton" (physicalDmg "s 2 / 1 +" 0) Club 1,
    melee "Sap" (physicalDmg "s 2 / 1 +" 0) Club 0,
    melee "Staff" (physicalDmg "s 2 / 2 +" 0) Club 2,
    melee "Stun Baton" (Dmg (DmgStun "6") Half) Club 1,
    -- Exotic Melee Weapons
    melee "Monofilament Whip" (physicalDmg "8" (-4)) Exotic 2,
    melee "Pole Arm" (physicalDmg "s 2 / 2 +" (-2)) Exotic 2,
    melee "Riot Shield" (Dmg (DmgStun "s 2 /") (AP 2)) Exotic 0,
    melee "Taser Armor/Shield" (Dmg (DmgStun "6") Half) Exotic 0,
    -- Unarmed
    melee "Shock Glove" (Dmg (DmgStun "5") Half) Unarmed 0,
    melee "Shock Frills" (Dmg (DmgStun "6") Half) Unarmed 0,
    -- Bows (Calculated in a separate Bow list)
    -- Crossbows   
    ranged "Light Crossbow" (dmg 3 0) Crossbow [] 4,
    ranged "Medium Crossbow" (dmg 5 0) Crossbow [] 4,
    ranged "Heavy Crossbow" (dmg 7 0) Crossbow [] 4,
    -- Throwing Weapons
    melee "Shuriken" (physicalDmg "s 2 /" 0) ThrowingWeapon 0,
    melee "Throwing Knife" (physicalDmg "s 2 / 1 +" 0) ThrowingWeapon 0,
    -- Tasers
    ranged "Defiance EX Shocker" (Dmg (DmgStun "8") Half) Taser [SingleShot] 4,
    ranged "Yamaha Pulsar" (Dmg (DmgStun "6") Half) Taser [SemiAutomatic] 4,
    -- Hold Outs
    ranged "Raecor Sting" (dmg 6 5) HoldOut [SingleShot] 5,
    ranged "Streetline Special" (dmg 4 0) HoldOut [SingleShot] 6,
    -- Light Pistols
    ranged "Colt America L36" (dmg 4 0) LightPistol [SemiAutomatic] 11,
    rranged "Fischetti Security 600 Light Pistol" (dmg 4 0) LightPistol [SemiAutomatic] 1 11,
    rranged "Hammerli 620S" (dmg 4 0) LightPistol [SemiAutomatic] 1 30,
    rranged "Yamaha Sakura Fubuki" (dmg 4 0) LightPistol [SemiAutomatic, BurstFire] 1 40,
    -- Heavy Pistols
    ranged "Ares Predator IV" (dmg 5 (-1)) HeavyPistol [SemiAutomatic] 15, 
    ranged "Ares Viper Silvergun" (dmg 8 5) HeavyPistol [SemiAutomatic, BurstFire] 30,
    ranged "Colt Manhunter" (dmg 5 (-1)) HeavyPistol [SemiAutomatic] 16,
    ranged "Remington Roomsweeper" (dmg 5 (-1)) HeavyPistol [SemiAutomatic] 8,
    ranged "Remington Roomsweeper w/ flechettes" (dmg 7 5) HeavyPistol [SemiAutomatic] 8,
    ranged "Ruger Super Warhawk" (dmg 6 (-2)) HeavyPistol [SingleShot] 6,
    -- Machine Pistols
    rranged "Ceska Black Scorpion" (dmg 4 0) MachinePistol [SemiAutomatic, BurstFire] 1 35,
    ranged "Steyr TMP" (dmg 4 0) MachinePistol [SemiAutomatic, BurstFire, Automatic] 30,
    -- Submachine Guns
    rranged "AK-97 Carbine" (dmg 5 0) SubMachineGun [SemiAutomatic, BurstFire, Automatic] 1 30,
    rranged "HK-227X" (dmg 5 0) SubMachineGun [SemiAutomatic, BurstFire, Automatic] 1 28,
    rranged "HK MP-5 TX" (dmg 5 0) SubMachineGun [SemiAutomatic, BurstFire, Automatic] 3 20,
    rranged "Ingram Smartgun X" (dmg 5 0) SubMachineGun [BurstFire, Automatic] 3 32,
    rranged "Uzi IV" (dmg 5 0) SubMachineGun [BurstFire] 1 24,
    -- Assault Rifles
    ranged "AK-97" (dmg 6 (-1)) AssaultRifle [SemiAutomatic, BurstFire, Automatic] 38,
    rranged "Ares Alpha" (dmg 6 (-1)) AssaultRifle [SemiAutomatic, BurstFire, Automatic] 2 42,
    ranged "Ares Alpha w/ Grenade Launcher" Grenade AssaultRifle [SingleShot] 6,
    rranged "FN HAR" (dmg 6 (-1)) AssaultRifle [SemiAutomatic, BurstFire, Automatic] 2 35,
    rranged "HK XM30" (dmg 6 (-1)) AssaultRifle [SemiAutomatic, BurstFire, Automatic] 1 30,
    ranged "HK XM30 w/ GrenadeLauncher" Grenade AssaultRifle [SingleShot] 8,
    rranged "HK XM30 Shotgun" (dmg 7 1) AssaultRifle [SemiAutomatic] 1 10,
    rranged "HK XM30 Carbine" (dmg 5 0) AssaultRifle [SemiAutomatic, BurstFire, Automatic] 1 30,
    rranged "HK XM30 Sniper" (dmg 7 (-2)) AssaultRifle [SemiAutomatic] 1 10,
    rranged "HK XM30 LMG" (dmg 6 (-1)) AssaultRifle [BurstFire, Automatic] 3 100,
    -- Sport Rifles
    rranged "Ruger 100" (dmg 7 (-1)) SportRifle [SemiAutomatic] 1 5,
    rranged "PJSS Elephant Rifle" (dmg 9 (-1)) SportRifle [SingleShot] 1 2,
    -- Sniper Rifles
    rranged "Ranger Arms SM-4" (dmg 8 (-3)) SniperRifle [SemiAutomatic] 1 15,
    rranged "Walter MA-2100" (dmg 7 (-3)) SniperRifle [SemiAutomatic] 1 10,
    -- Shotguns
    ranged "Mossberg AM-CMDT" (dmg 9 5) Shotgun [SemiAutomatic, BurstFire, Automatic] 10,
    rranged "Remington 990" (dmg 7 (-1)) Shotgun [SemiAutomatic] 1 8,
    rranged "Remington 990 w/ flechettes" (dmg 9 5) Shotgun [SemiAutomatic] 1 8,
    -- Special Weapons
    ranged "Ares S-III Super Squirt" Chemical Special [SemiAutomatic] 20,
    ranged "Fischetti Pain Inducer" (SpecialDmg Half) Special [SingleShot] 0,
    ranged "Parashield Dart Pistol" (Toxin (AP (-2))) Special [SemiAutomatic] 5,
    ranged "Parashield Dart Rifle" (Toxin (AP (-2))) Special [SemiAutomatic] 10,
    -- Light Machine Guns
    rranged "Ingram White Knight" (dmg 6 (-1)) LightMachineGun [BurstFire, Automatic] 6 100,
    -- Medium Machine Gun
    ranged "Stoner-Ares M202" (dmg 6 (-2)) MediumMachineGun [Automatic] 100, 
    -- Heavy Machine Gun
    rranged "Ultimax HMG-2" (dmg 7 (-3)) HeavyMachineGun [Automatic] 10 100,
    -- Assault Cannons
    rranged "Panther XXL" (dmg 10 (-5)) AssaultCannon [SingleShot] 1 15,
    -- Grenade Launcher
    ranged "Ares Antioch-2" Grenade GrenadeLauncher [SingleShot] 8,
    ranged "ArmTech MGL-12" Grenade GrenadeLauncher [SemiAutomatic] 12,
    -- Missile Launchers
    ranged "Aztechnology Striker" Missile MissileLauncher [SingleShot] 1,
    ranged "Mitsubishi Yakusoku MRL" Missile MissileLauncher [SemiAutomatic] 8]

bow :: String -> Damage -> Int -> Weapon
bow name damage minStrength = Weapon name damage Bow Nothing Nothing (Just [SingleShot]) (Just 1) (Just minStrength)

bowList :: [Weapon]
bowList = [bow ("Bow (Rating " ++ show x ++ ")") (dmg (x + 2) 0) x | x <- [1..12]]  
