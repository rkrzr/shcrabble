module Lib
  ( distanceToMiddle
  , avgDistanceToMiddle
  , getAllPlacementOptions
  , getAllPossiblePlacements
  , getAvailablePlacedPieces
  , getFittingWords
  , insertPlacedPieces
  , optionsInfo
  , placeFirstWord
  , readWordFile
  , writePlayingField
  ) where

import SVG
import Types
       (Coordinates(..), Direction(..), MoveType(..), Options(..),
        PlacedPiece(..), PlayingField, distanceToMiddle)

import Data.Semigroup ((<>))
import Options.Applicative
       (Parser, ParserInfo, (<**>), argument, fullDesc, header, help,
        helper, info, long, metavar, short, str, switch)

import qualified Data.Map.Strict as Map

-- Parsing of command-line arguments
optionsParser :: Parser Options
optionsParser =
  Options <$>
  switch (long "generate-svg-per-turn" <> short 'g' <> help "Generate an svg for each turn.") <*>
  argument str (metavar "INPUT_FILE" <> help "File to read the wordlist from.") <*>
  argument str (metavar "OUTPUT_FILE" <> help "File to write the final SVG to.")

optionsInfo :: ParserInfo Options
optionsInfo = info parser description
  where
    parser = optionsParser <**> helper
    description = fullDesc <> header "Shcrabble - generate Scrabble-boards from any text file!"

-- we limit ourselves to only ASCII chars fow now
lowerAlphabet :: [Char]
lowerAlphabet = ['a' .. 'z']

upperAlphabet :: [Char]
upperAlphabet = ['A' .. 'Z']

allowedCharacters :: [Char]
allowedCharacters = lowerAlphabet ++ upperAlphabet

readWordFile :: FilePath -> IO [String]
readWordFile path = do
  content <- readFile path
  let messyWords = concatMap words $ lines content
      cleanWords = map (filter (`elem` allowedCharacters)) messyWords
      longerWords = filter (\x -> length x > 1) cleanWords
  return longerWords

-- Game code
-- we always place the first word starting from (1,1) to the right
placeFirstWord :: String -> PlayingField -> PlayingField
placeFirstWord [] pf = pf
placeFirstWord xs pf = placeFirstWord' xs pf (C (1, 1))

placeFirstWord' :: String -> PlayingField -> Coordinates -> PlayingField
placeFirstWord' [] pf _ = pf
placeFirstWord' (z:zs) pf cs@(C (x, y)) = placeFirstWord' zs newPlayingField (C (x + 1, y))
  where
    newPlayingField = Map.insert cs (PlacedPiece z cs) pf

writePlayingField :: FilePath -> PlayingField -> IO ()
writePlayingField filePath pf = writeFile filePath (generatePlayingFieldSVG pf)

insertPlacedPieces :: [PlacedPiece] -> PlayingField -> PlayingField
insertPlacedPieces [] pf = pf
insertPlacedPieces (pp@(PlacedPiece _ cs):pps) pf =
  case Map.lookup cs pf of
    Nothing -> insertPlacedPieces pps (Map.insert cs pp pf)
    Just pp' ->
      case pp == pp' of
        True -> insertPlacedPieces pps (Map.insert cs pp pf)
        False -> error $ "The given piece " ++ show pp ++ " is invalid. " ++ show pp'

-- get all placed pieces where a word could be attached, i.e.
-- all pieces where either the x or the y-axis is still free
getAvailablePlacedPieces :: PlayingField -> [PlacedPiece]
getAvailablePlacedPieces pf = filter (isPieceAvailable pf) (Map.elems pf)

-- a piece is available if either its left and right or upper and lower
-- neigbors are free
isPieceAvailable :: PlayingField -> PlacedPiece -> Bool
isPieceAvailable pf (PlacedPiece _ cs) = isVerticalFree || isHorizontalFree
  where
    isVerticalFree = emptyNeighbors cs [Up, Down] pf
    isHorizontalFree = emptyNeighbors cs [L, R] pf

-- check if the neighbors in the given directions are all empty
emptyNeighbors :: Coordinates -> [Direction] -> PlayingField -> Bool
emptyNeighbors cs ds pf = all (isNeighborEmpty cs pf) ds

isNeighborEmpty :: Coordinates -> PlayingField -> Direction -> Bool
isNeighborEmpty cs pf d = Map.notMember (goOne d cs) pf

goOne :: Direction -> Coordinates -> Coordinates
goOne Up (C (x, y)) = (C (x, y + 1))
goOne Down (C (x, y)) = (C (x, y - 1))
goOne L (C (x, y)) = (C (x - 1, y))
goOne R (C (x, y)) = (C (x + 1, y))

-- check if the given word does not touch or overlap with conflicting pieces
isWordFitting :: PlayingField -> (MoveType, [PlacedPiece]) -> Bool
isWordFitting _pf (_, []) = True
-- TODO: Placed pieces are *not* ordered - we must make sure that pp is actually the first
-- character of the word!
isWordFitting pf (mt, (pp:pps)) =
  isFirstValid && (all (isEqualOrEmpty pf mt) (init pps)) && isLastValid
    -- Note: If the field is empty, then all three surrounding fields must be empty
    -- If the field is not empty, then only the field in the same direction must be empty
  where
    isFieldValid (PlacedPiece c cs) d ds =
      case Map.lookup cs pf of
        Just (PlacedPiece c' _) -> c == c' && isNeighborEmpty cs pf d
        Nothing -> emptyNeighbors cs ds pf
    isFirstValid =
      case mt of
        Horizontal -> isFieldValid pp L [Up, Down, L]
        Vertical -> isFieldValid pp Down [L, R, Down]
    isLastValid =
      case mt of
        Horizontal -> isFieldValid (last pps) R [Up, Down, R]
        Vertical -> isFieldValid (last pps) Up [L, R, Up]

isEqualOrEmpty :: PlayingField -> MoveType -> PlacedPiece -> Bool
isEqualOrEmpty pf mt (PlacedPiece c cs) =
  case Map.lookup cs pf of
    Nothing ->
      case mt of
        Horizontal -> emptyNeighbors cs [Up, Down] pf
        Vertical -> emptyNeighbors cs [L, R] pf
    Just (PlacedPiece c' cs') ->
      case mt of
        Horizontal -> c == c' && emptyNeighbors cs' [L, R] pf
        Vertical -> c == c' && emptyNeighbors cs' [Up, Down] pf

-- For the given word, return *all* possible placements given the contraints of the playing field
getFittingWords :: PlayingField -> String -> PlacedPiece -> [[PlacedPiece]]
getFittingWords _ [] _ = []
getFittingWords pf word pp = map snd fittingPlacements
  where
    possiblePlacements = getAllPossiblePlacements word pp
    fittingPlacements = filter (isWordFitting pf) possiblePlacements

-- It's often possible to attach a word in several ways to a given piece
-- think e.g. about words that contain the character of the placed piece several times
getAllPossiblePlacements :: String -> PlacedPiece -> [(MoveType, [PlacedPiece])]
getAllPossiblePlacements word pp = horizontalPlacements ++ verticalPlacements
  where
    horizontalPlacements = zip (repeat Horizontal) $ getDirectionalPlacements word Horizontal pp
    verticalPlacements = zip (repeat Vertical) $ getDirectionalPlacements word Vertical pp

getDirectionalPlacements :: String -> MoveType -> PlacedPiece -> [[PlacedPiece]]
getDirectionalPlacements word mt pp = getDirectionalPlacements' [] word mt pp

-- Note: We are generating all theoretical options here, they may not be possible
-- in practice, if there are already different pieces on the playing field
getDirectionalPlacements' :: String -> String -> MoveType -> PlacedPiece -> [[PlacedPiece]]
getDirectionalPlacements' _prefix [] _ _ = []
getDirectionalPlacements' prefix (x:xs) mt pp@(PlacedPiece c _cs) =
  case x == c
  -- skip characters that don't match
        of
    False -> getDirectionalPlacements' (prefix ++ [x]) xs mt pp
    True ->
      case mt
    -- place the prefix to the left and the suffix to the right of (x,y)
    -- we have to reverse twice: Once to insert in single steps and once to make the order right again
            of
        Horizontal ->
          let placedPrefix = placeString L (reverse prefix) pp
              pps = reverse placedPrefix ++ [pp] ++ placeString R xs pp
          in pps : getDirectionalPlacements' (prefix ++ [x]) xs mt pp
                  -- Note: Down is Up and Up is Down, thanks to SVG's weird coordinate system
        Vertical ->
          let placedPrefix = placeString Down (reverse prefix) pp
              pps = reverse placedPrefix ++ [pp] ++ placeString Up xs pp
          in pps : getDirectionalPlacements' (prefix ++ [x]) xs mt pp

placeString :: Direction -> String -> PlacedPiece -> [PlacedPiece]
placeString _ [] _ = []
placeString d (w:ws) (PlacedPiece _ cs) = pp' : placeString d ws pp'
  where
    cs' = goOne d cs
    pp' = PlacedPiece w cs'

-- find all possible placements for the given word and the given placed piece
getAllPlacementOptions :: String -> PlayingField -> PlacedPiece -> [(String, [PlacedPiece])]
getAllPlacementOptions w pf pp = map (\pps -> (w, pps)) $ getFittingWords pf w pp

avgDistanceToMiddle :: [PlacedPiece] -> Double
avgDistanceToMiddle [] = 0
avgDistanceToMiddle pps = sum (map distanceToMiddle pps) / fromIntegral (length pps)
