module Main where

import Lib
import SVG
import Types

import Control.Monad (unless, when)
import Data.Ord (comparing)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (delete, minimumBy, sortBy)
import Debug.Trace (trace, traceShowId)
import Options.Applicative (execParser)
import System.Directory (doesFileExist)
import System.Environment (getArgs)


allDirections = [L ..]

-- we always place the first word starting from (1,1) to the right
placeFirstWord :: String -> PlayingField -> PlayingField
placeFirstWord [] pf = pf
placeFirstWord xs pf = placeFirstWord' xs pf (1,1)


placeFirstWord' :: String -> PlayingField -> Coordinates -> PlayingField
placeFirstWord' [] pf _         = pf
placeFirstWord' (z:zs) pf cs@(x,y) = placeFirstWord' zs newPlayingField (x+1, y)
  where newPlayingField = Map.insert cs (PlacedPiece z cs) pf


writePlayingField :: FilePath -> PlayingField -> IO ()
writePlayingField filePath pf = writeFile filePath (generatePlayingFieldSVG pf)


getNeighbors :: PlayingField -> Coordinates -> Map.Map Direction (Maybe PlacedPiece)
getNeighbors pf (x,y) = Map.map (\cs -> Map.lookup cs pf) neighborMap
  where
    neighborCoordinates = [(L, (x-1, y)), (R, (x+1, y)), (Up, (x, y+1)), (Down, (x, y-1))]
    neighborMap = Map.fromList neighborCoordinates


-- find all words in the bag that could be attached to the given character
getMatchingWords :: Bag -> PlacedPiece -> (PlacedPiece, [String])
getMatchingWords [] pp                   = (pp, [])
getMatchingWords xs pp@(PlacedPiece c _) = (pp, filter (elem c) xs)


getDirectonalPlacements :: String -> MoveType -> PlacedPiece -> [[PlacedPiece]]
getDirectonalPlacements word mt pp = getDirectonalPlacements' [] word mt pp


-- Note: We are generating all theoretical options here, they may not be possible
-- in practice, if there are already different pieces on the playing field
getDirectonalPlacements' :: String -> String -> MoveType -> PlacedPiece -> [[PlacedPiece]]
getDirectonalPlacements' prefix [] mt pp@(PlacedPiece c cs) = []
getDirectonalPlacements' prefix (x:xs) mt pp@(PlacedPiece c cs) = case x == c of
  -- skip characters that don't match
  False -> getDirectonalPlacements' (prefix ++ [x]) xs mt pp
  True  -> case mt of
    -- place the prefix to the left and the suffix to the right of (x,y)
    -- we have to reverse twice: Once to insert in single steps and once to make the order right again
    Horizontal -> let placedPrefix = placeString L (reverse prefix) pp
                      pps = reverse placedPrefix ++ [pp] ++ placeString R xs pp
                  in pps : getDirectonalPlacements' (prefix ++ [x]) xs mt pp
                  -- Note: Down is Up and Up is Down, thanks to SVG's weird coordinate system
    Vertical   -> let placedPrefix = placeString Down (reverse prefix) pp
                      pps = reverse placedPrefix ++ [pp] ++ placeString Up xs pp
                  in pps : getDirectonalPlacements' (prefix ++ [x]) xs mt pp


-- For the given word, return *all* possible placements given the contraints of the playing field
getFittingWords :: PlayingField -> String -> PlacedPiece -> [[PlacedPiece]]
getFittingWords pf [] pp = []
getFittingWords pf word pp = map snd fittingPlacements
  where
    possiblePlacements = getAllPossiblePlacements word pp
    fittingPlacements = filter (isWordFitting pf) possiblePlacements


-- check if the given word does not touch or overlap with conflicting pieces
isWordFitting :: PlayingField -> (MoveType, [PlacedPiece]) -> Bool
isWordFitting pf (_, [])   = True
-- TODO: Placed pieces are *not* ordered
isWordFitting pf (mt, (pp@(PlacedPiece c cs):pps)) =
  isFirstValid && (all (isEqualOrEmpty pf mt) (init pps)) && isLastValid
  where
    -- Note: If the field is empty, then all three surrounding fields must be empty
    -- If the field is not empty, then only the field in the same direction must be empty
    isFieldValid (PlacedPiece c cs) d ds = case Map.lookup cs pf of
      Just (PlacedPiece c' cs') -> c == c' && isNeighborEmpty cs pf d
      Nothing                   -> emptyNeighbors cs ds pf
    isFirstValid = case mt of
      Horizontal -> isFieldValid pp L [Up, Down, L]
      Vertical   -> isFieldValid pp Down [L, R, Down]
    isLastValid  = case mt of
      Horizontal -> isFieldValid (last pps) R [Up, Down, R]
      Vertical   -> isFieldValid (last pps) Up [L, R, Up]


isEqualOrEmpty :: PlayingField -> MoveType -> PlacedPiece -> Bool
isEqualOrEmpty pf mt (PlacedPiece c cs) = case Map.lookup cs pf of
  Nothing -> case mt of
    Horizontal -> emptyNeighbors cs [Up, Down] pf
    Vertical   -> emptyNeighbors cs [L, R] pf
  Just (PlacedPiece c' cs') -> case mt of
    Horizontal -> c == c' && emptyNeighbors cs' [L, R] pf
    Vertical   -> c == c' && emptyNeighbors cs' [Up, Down] pf


-- check if the neighbors in the given directions are all empty
emptyNeighbors :: Coordinates -> [Direction] -> PlayingField -> Bool
emptyNeighbors cs ds pf = all (isNeighborEmpty cs pf) ds


isNeighborEmpty :: Coordinates -> PlayingField -> Direction -> Bool
isNeighborEmpty cs pf d = Map.notMember (goOne d cs) pf


-- It's often possible to attach a word in several ways to a given piece
-- think e.g. about words that contain the character of the placed piece several times
getAllPossiblePlacements :: String -> PlacedPiece -> [(MoveType, [PlacedPiece])]
getAllPossiblePlacements word pp = horizontalPlacements ++ verticalPlacements
  where
    horizontalPlacements = zip (repeat Horizontal) $ getDirectonalPlacements word Horizontal pp
    verticalPlacements   = zip (repeat Vertical) $ getDirectonalPlacements word Vertical pp


goOne :: Direction -> Coordinates -> Coordinates
goOne Up   (x,y) = (x, y+1)
goOne Down (x,y) = (x, y-1)
goOne L    (x,y) = (x-1, y)
goOne R    (x,y) = (x+1, y)


placeString :: Direction -> String -> PlacedPiece -> [PlacedPiece]
placeString _ [] _ = []
placeString d (w:ws) (PlacedPiece c cs) = pp' : placeString d ws pp'
  where
    cs' = goOne d cs
    pp' = PlacedPiece w cs'


-- get all placed pieces where a word could be attached, i.e.
-- all pieces where either the x or the y-axis is still free
getAvailablePlacedPieces :: PlayingField -> [PlacedPiece]
getAvailablePlacedPieces pf = filter (isPieceAvailable pf) (Map.elems pf)


-- a piece is available if either its left and right or upper and lower
-- neigbors are free
isPieceAvailable :: PlayingField -> PlacedPiece -> Bool
isPieceAvailable pf (PlacedPiece c cs) = isVerticalFree || isHorizontalFree
  where
    neighbors = getNeighbors pf cs
    isVerticalFree   = emptyNeighbors cs [Up, Down] pf
    isHorizontalFree = emptyNeighbors cs [L, R] pf


executeTurn :: PlayingField -> Bag -> Maybe (PlayingField, Bag)
executeTurn pf [] = Nothing  -- end of the game
executeTurn pf ws = case sortedPlacements of
  []           -> Nothing
  ((w, pps):_) -> Just (insertPlacedPieces pps pf, delete w ws)
  where
    -- all placed pieces where a word could be attached
    availablePlacedPieces = getAvailablePlacedPieces pf
    wordsAndPieces = [(w, pp) | w <- ws, pp <- availablePlacedPieces]
    allPlacementOptions = concatMap (\(w, pp) -> getAllPlacementOptions w pf pp) wordsAndPieces
    viablePlacementOptions = filter (\(w, pps) -> not (null pps)) allPlacementOptions
    -- find the placement the closest to the center of the playing field
    sortedPlacements = sortBy (\(w1, pps1) (w2, pps2) -> comparing avgDistanceToMiddle pps1 pps2) viablePlacementOptions


-- find all possible placements for the given word and the given placed piece
getAllPlacementOptions :: String -> PlayingField -> PlacedPiece -> [(String, [PlacedPiece])]
getAllPlacementOptions w pf pp = map (\pps -> (w, pps)) $ getFittingWords pf w pp


-- executeTurn :: PlayingField -> Bag -> Maybe (PlayingField, Bag)
-- executeTurn pf []      = Nothing  -- end of the game
-- executeTurn pf (w:ws)  = case viablePlacementOptions w of
--   [] -> executeTurn pf ws  -- try the next word in the bag
--   -- we simply pick the first viable placement option for a word
--   -- this could be tuned later
--   ((pp, (pps:_)):xs) -> Just (insertPlacedPieces pps pf, ws)
--   where
--     -- all placed pieces where a word could be attached
--     availablePlacedPieces = getAvailablePlacedPieces pf
--     allPlacementOptions w = map (\pp -> getFittingWords pf pp w) availablePlacedPieces
--     viablePlacementOptions w = filter (\(pp, pps) -> not (null pps)) (allPlacementOptions w)


distanceToMiddle :: PlacedPiece -> Double
distanceToMiddle (PlacedPiece _ (x,y)) = sqrt $ centerX ** 2 + centerY ** 2
  where
    -- (x,y) is the top-right corner of a piece, and we have a sidelength of 1
    centerX = fromIntegral x - 0.5
    centerY = fromIntegral y - 0.5


avgDistanceToMiddle :: [PlacedPiece] -> Double
avgDistanceToMiddle [] = 0
avgDistanceToMiddle pps =  sum (map distanceToMiddle pps) / fromIntegral (length pps)


insertPlacedPieces :: [PlacedPiece] -> PlayingField -> PlayingField
insertPlacedPieces [] pf = pf
insertPlacedPieces (pp@(PlacedPiece c cs):pps) pf = case Map.lookup cs pf of
  Nothing  -> insertPlacedPieces pps (Map.insert cs pp pf)
  Just pp' -> case pp == pp' of
    True  -> insertPlacedPieces pps (Map.insert cs pp pf)
    False -> error $ "The given piece " ++ show pp ++ " is invalid. " ++ show pp'


executeGame :: Options -> PlayingField -> Bag -> IO PlayingField
executeGame os pf bag = executeGame' os pf bag 1


executeGame' :: Options -> PlayingField -> Bag -> Int -> IO PlayingField
executeGame' _  pf []  _    = return pf
executeGame' os pf bag turn = do
  let filePath = (oSvgFile os) ++ show turn ++ ".svg"
  -- write an SVG file for each turn if requested
  _ <- when (oGenerateSvgPerTurn os) (writePlayingField filePath pf)
  let maybeEndOfGame = executeTurn pf bag
  case maybeEndOfGame of
    Nothing          -> putStrLn "The End." >> return pf
    Just (pf', bag') -> executeGame' os pf' bag' (turn + 1)


main :: IO ()
main = do
  options <- execParser optionsInfo
  fileExists <- doesFileExist (oWordFile options)
  if fileExists
    then do
      allWords <- readWordFile (oWordFile options)
      let (firstWord:remainingWords) = allWords
          playingField = placeFirstWord firstWord Map.empty
          outputFilename = (oSvgFile options) ++ ".svg"
      -- putStrLn $ "allWords: " ++ show allWords

      playingField' <- executeGame options playingField remainingWords
      writePlayingField outputFilename playingField'
    else do
      putStrLn ("Word file does not exist: " ++ (oWordFile options))
