{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad
import Data.List (sortBy)
import System.Random

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

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = let attackWith = min 3 (attackers bf - 1)
                defendWith = min 2 (defenders bf)
                getDie x = liftM (sortBy $ flip compare) replicateM x die
                attackDie = getDie attackWith
                defendDie = getDie defendWith
                go :: Army
                   -> Army
                   -> Rand StdGen [DieValue]
                   -> Rand StdGen [DieValue]
                   -> Rand StdGen Battlefield
                go a d (Rand StdGen []) _ = Battlefield a d
                go a d _ (Rand StdGen []) = Battlefield a d
                go a d (a1:as) (d1:ds)
                   | a1 > d1 = go a (d-1) as ds
                   | otherwise = go (a-1) d as ds
            in go attackWith defendWith attackDie defendDie
