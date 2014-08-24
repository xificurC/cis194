{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad
import Data.List (sortBy)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
                 deriving (Show)

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = let attackWith = min 3 (attackers bf - 1)
                defendWith = min 2 (defenders bf)
                getDie x = liftM (sortBy $ flip compare) (replicateM x die)
                attackDie = getDie attackWith
                defendDie = getDie defendWith
                go a d [] _ = Battlefield a d
                go a d _ [] = Battlefield a d
                go a d (a1:as) (d1:ds)
                    | a1 > d1 = go a (d-1) as ds
                    | otherwise = go (a-1) d as ds
            in liftM2 (go (attackers bf) (defenders bf)) attackDie defendDie
               
invade :: Battlefield -> Rand StdGen Battlefield
invade bf = battle bf >>= \r ->
            case r of
              (Battlefield 1 _) -> return r
              (Battlefield _ 0) -> return r
              _ -> invade r

successProb :: Battlefield -> Rand StdGen Double
successProb bf = replicateM 1000 (invade bf) >>= \l ->
                 return $
                 (/1000) . fromIntegral . length . filter ((>1) . attackers) $ l
