module Types (
  Coordinates,
  PlayingField
) where

import qualified Data.Map as Map

type Coordinates = (Int, Int)

-- each playing piece is simply a character
type PlayingField = Map.Map Coordinates Char

