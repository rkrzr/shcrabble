module Types (
  Coordinates,
  Direction(..),
  PlacedPiece(..),
  PlayingField
) where

import qualified Data.Map as Map

type Coordinates = (Int, Int)

data Direction = L | R | Up | Down
  deriving (Eq, Enum, Ord, Show)

-- each playing piece is simply a character
-- newtype Piece = Piece Char

-- it's convenient to have a piece know its coordinates
data PlacedPiece = PlacedPiece Char Coordinates
  deriving (Eq, Show)

type PlayingField = Map.Map Coordinates PlacedPiece

