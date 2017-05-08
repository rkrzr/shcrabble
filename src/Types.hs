module Types (
  Bag,
  Coordinates,
  Direction(..),
  MoveType(..),
  PlacedPiece(..),
  PlayingField
) where

import qualified Data.Map as Map

type Coordinates = (Int, Int)

-- the bag of remaining words
type Bag = [String]

data Direction = L | R | Up | Down
  deriving (Eq, Enum, Ord, Show)

data MoveType = Horizontal | Vertical
  deriving (Eq, Enum, Ord, Show)

-- each playing piece is simply a character
-- newtype Piece = Piece Char

-- it's convenient to have a piece know its coordinates
data PlacedPiece = PlacedPiece {ppChar :: Char, ppCoordinates :: Coordinates}
  deriving (Eq, Show)

type PlayingField = Map.Map Coordinates PlacedPiece

