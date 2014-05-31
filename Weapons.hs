module Weapons 
    (Magazine, 
    WeaponType, 
    Weapon,
    Damage (Dmg),
    weaponDb,
    getWeapon,
    dmg,
    getDamage,
    showWeapon 
) where

import Data.Map hiding (foldl)
import Stats
import Data.List

data AP = AP Int | Half deriving (Show)
data Damage = Dmg { 
    dmgPhysical :: (Int -> Int), 
    dmgStun :: Maybe (Int -> Int), 
    dmgAP :: AP} 
    | Grenade 
    | Chemical 
    | SpecialDmg AP
    | Toxin AP
    | Missile

instance Show Damage where
    show (Dmg _ _ (AP ap)) = show "AP: " ++ (show ap)
    show (Dmg _ _ (Half)) = show "AP: -Half"
    show Grenade = show "Grenade"
    show Chemical = show "Chemical"
    show (SpecialDmg ap) = show "Special" ++ (show ap)
    show (Toxin ap) = show "Toxin" ++ (show ap)
    show Missile = show "Missile"

getDamage stats (Dmg physical stun ap) =
    let str = statStrength stats
        getStun (Just x) = x str
        getStun (Nothing) = 0
    in
    (physical str, getStun stun, ap) 

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
    weaponAmmoCapacity :: Maybe Int} deriving (Show)

showWeapon stats wpn =
    let (physical, stun, ap) = getDamage stats (weaponDmg wpn)
        weaponStats = [weaponName wpn, " Physical: ", show physical, " Stun: ", show stun, " AP: ", show ap]
    in Data.List.foldl (++) "" weaponStats

ranged name damage wpnType modes ammoCapacity = Weapon name damage wpnType Nothing Nothing (Just modes) (Just ammoCapacity)
rranged name damage wpnType modes recoil ammoCapacity = Weapon name damage wpnType Nothing (Just recoil) (Just modes) (Just ammoCapacity)
melee name damage wpnType reach = Weapon name damage wpnType (Just reach) Nothing Nothing Nothing 

dmg hp ap = Dmg (\x -> hp) Nothing (AP ap)
physicalDmg f ap = Dmg f Nothing (AP ap)

div2 x = x `div` 2

notFoundWeapon x = melee ("Could not find '" ++ x ++ "' in weapon database.") (dmg 0 0) Blade 0
getWeapon x = findWithDefault (notFoundWeapon x) x weaponDb
weaponDb = fromList [(weaponName x, x) | x <- [
    -- Blades
    melee "Combat Axe" (physicalDmg (\str -> 4 + div2 str) (-1)) Blade 2,
    melee "Forearm Snap Blades" (physicalDmg (\str -> 2 + div2 str) 0) Blade 0,
    melee "Katana" (physicalDmg (\str -> 3 + div2 str) (-1)) Blade 1,
    melee "Knife" (physicalDmg (\str -> 1 + div2 str) 0) Blade 0,
    melee "Monofilament Sword" (physicalDmg (\str -> 3 + div2 str) (-1)) Blade 1,
    melee "Survival Knife" (physicalDmg (\str -> 1 + div2 str) (-1)) Blade 0,
    melee "Sword" (physicalDmg (\str -> 3 + div2 str) 0) Blade 1,
    --Clubs
    melee "Club" (physicalDmg (\str -> 1 + div2 str) 0) Club 1,
    melee "Extendable Baton" (physicalDmg (\str -> 1 + div2 str) 0) Club 1,
    melee "Sap" (physicalDmg (\str -> 1 + div2 str) 0) Club 0,
    melee "Staff" (physicalDmg (\str -> 2 + div2 str) 0) Club 2,
    melee "Stun Baton" (Dmg (\_ -> 0) (Just (\str -> 6)) Half) Club 1,
    -- Exotic Melee Weapons
    melee "Monofilament Whip" (physicalDmg (\str -> 8) (-4)) Exotic 2,
    melee "Pole Arm" (physicalDmg (\str -> 2 + div2 str) (-2)) Exotic 2,
    melee "Riot Shield" (Dmg (\_ -> 0) (Just (\str -> div2 str)) (AP 2)) Exotic 0,
    melee "Taser Armor/Shield" (Dmg (\_ -> 0) (Just (\str -> 6)) Half) Exotic 0,
    -- Unarmed
    melee "Shock Glove" (Dmg (\_ -> 0) (Just (\str -> 5)) Half) Unarmed 0,
    melee "Shock Frills" (Dmg (\_ -> 0) (Just (\str -> 6)) Half) Unarmed 0,
    -- Bows
    -- Bow (damage STR Min + 2)P (AP -)
    -- Crossbows   
    ranged "Light Crossbow" (dmg 3 0) Crossbow [] 4,
    ranged "Medium Crossbow" (dmg 5 0) Crossbow [] 4,
    ranged "Heavy Crossbow" (dmg 7 0) Crossbow [] 4,
    -- Throwing Weapons
    melee "Shuriken" (physicalDmg (\str -> div2 str) 0) ThrowingWeapon 0,
    melee "Throwing Knife" (physicalDmg (\str -> 1 + div2 str) 0) ThrowingWeapon 0,
    -- Tasers
    ranged "Defiance EX Shocker" (Dmg (\_ -> 0) (Just (\str -> 8)) Half) Taser [SingleShot] 4,
    ranged "Yamaha Pulsar" (Dmg (\_ -> 0) (Just (\str -> 6)) Half) Taser [SemiAutomatic] 4,
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
    ranged "Mitsubishi Yakusoku MRL" Missile MissileLauncher [SemiAutomatic] 8]]

