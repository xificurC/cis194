{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []
               
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = liftA2 (:) p (zeroOrMore p)

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

infixr 5 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

ident :: Parser String
-- ident = liftA2 (:) (satisfy isAlpha) (zeroOrMore (satisfy isAlphaNum))
ident = satisfy isAlpha <:> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

getIdent :: Parser Ident
getIdent = ident

getN :: Parser Atom
getN = N <$> posInt

getI :: Parser Atom
getI = I <$> getIdent
       
getAtom :: Parser Atom
getAtom = getN <|> getI

getA :: Parser SExpr
getA = A <$> getAtom

-- getComb :: Parser SExpr
-- getComb = Comb <$> (char '(' *> getComb <* char ')')
