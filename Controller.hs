module Controller (Direccio(..), viToDireccio) where

import Data.Char (toLower)


data Direccio = Oest | Sud | Nord | Est deriving (Eq)

instance Show Direccio where
  show Oest = "esquerra"
  show Sud  = "avall"
  show Nord = "amunt"
  show Est  = "dreta"

-- maps vi keys to Direccio
viToDireccio :: Char -> Maybe Direccio
viToDireccio c
  | c' == 'h'   = Just Oest
  | c' == 'j'   = Just Sud
  | c' == 'k'   = Just Nord
  | c' == 'l'   = Just Est
  | otherwise   = Nothing
  where c' = toLower c

