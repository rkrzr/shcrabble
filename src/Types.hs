module Types
  ( Bag
  , Coordinates(..)
  , Direction(..)
  , MoveType(..)
  , Options(..)
  , PlacedPiece(..)
  , PlayingField
  , distanceToMiddle
  ) where

import qualified Data.Map.Strict as Map

newtype Coordinates = C
  { cCoordinates :: (Int, Int)
  } deriving (Eq)

-- the bag of remaining words
type Bag = [String]

data Direction
  = L
  | R
  | Up
  | Down
  deriving (Eq, Enum, Ord, Show)

data MoveType
  = Horizontal
  | Vertical
  deriving (Eq, Enum, Ord, Show)

-- each playing piece is simply a character
-- newtype Piece = Piece Char
-- it's convenient to have a piece know its coordinates
data PlacedPiece = PlacedPiece
  { ppChar :: Char
  , ppCoordinates :: Coordinates
  } deriving (Eq)

type PlayingField = Map.Map Coordinates PlacedPiece

-- command-line arguments
data Options = Options
  { oGenerateSvgPerTurn :: Bool
  , oWordFile :: FilePath
  , oSvgFile :: FilePath
  }

-- keep visual noise to a minimum
instance Show Coordinates where
  show (C cs) = show cs

-- show the character and the distance to the middle
instance Show PlacedPiece where
  show pp@(PlacedPiece c _cs) = "PP " ++ [c, ' '] ++ (show $ distanceToMiddle pp)

-- Important: Coordinates are used as the key in the PlayingField, which is a
-- Data.Map. Since Data.Map is implemented as a tree it requires a *total* ordering
-- of keys, otherwise two different keys but with the same ordering will overwrite
-- each other!
-- We want to use the distance to the center as an index, but since this value is
-- *not* unique, we must make sure that we first order on the distance and second
-- order on the coordinates themselves (i.e. first on the x-coordinate and then on
-- the y-coordinate). This way we *can* guarantee a total ordering *and* have our
-- index.
instance Ord Coordinates where
  c1 `compare` c2 =
    case cDistanceToMiddle c1 `compare` cDistanceToMiddle c2 of
      LT -> LT
      GT -> GT
      EQ ->
        case c1 == c2 of
          True -> EQ
          False -> cCoordinates c1 `compare` cCoordinates c2

distanceToMiddle :: PlacedPiece -> Double
distanceToMiddle (PlacedPiece _ (C (x, y))) = sqrt $ centerX ** 2 + centerY ** 2
    -- (x,y) is the top-right corner of a piece, and we have a sidelength of 1
    -- TODO: Fix this for negative indices
  where
    centerX = fromIntegral x -- 0.5
    centerY = fromIntegral y -- 0.5

cDistanceToMiddle :: Coordinates -> Double
cDistanceToMiddle (C (x, y)) = sqrt $ fromIntegral $ x * x + y * y
