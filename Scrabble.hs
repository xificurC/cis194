{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where

import Data.Monoid
import qualified Data.Map.Strict as M
import Data.Char (toLower)

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

instance Monoid Score where
    mempty = Score 0
    mappend = (+)
              
getScore :: Score -> Int
getScore (Score x) = x
              
charScore :: M.Map Char Int
charScore = M.fromList [('a',1),('b',3),('c',3),('d',2),('e',1),('f',4)
                       ,('g',2),('h',4),('i',1),('j',8),('k',5),('l',1)
                       ,('m',3),('n',1),('o',1),('p',3),('q',10),('r',1)
                       ,('s',1),('t',1),('u',1),('v',4),('w',4),('x',8)
                       ,('y',4),('z',10)]

score :: Char -> Score
score c = Score (M.findWithDefault 0 (toLower c) charScore)
          
scoreString :: String -> Score
scoreString = foldr mappend mempty . map score
