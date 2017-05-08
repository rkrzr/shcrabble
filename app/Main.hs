module Main where

import SVG
import Trie
import Types
import Data.Ord (comparing)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (delete, sortBy)
import Debug.Trace (trace, traceShowId)
import System.Environment (getArgs)

allDirections = [L ..]

lowerAlphabet = ['a' .. 'z']
upperAlphabet = ['A' .. 'Z']
allowedCharacters = lowerAlphabet ++ upperAlphabet


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
    Horizontal -> let pps = placeString L (reverse prefix) pp ++ [pp] ++ placeString R xs pp
                  in pps : getDirectonalPlacements' (prefix ++ [x]) xs mt pp
                  -- Note: Down is Up and Up is Down, thanks to SVG's weird coordinate system
    Vertical   -> let pps = placeString Down (reverse prefix) pp ++ [pp] ++ placeString Up xs pp
                  in pps : getDirectonalPlacements' (prefix ++ [x]) xs mt pp


-- For the given word, return *all* possible placements given the contraints of the playing field
getFittingWords :: PlayingField -> PlacedPiece -> String -> (PlacedPiece, [[PlacedPiece]])
getFittingWords pf pp []   = (pp, [])
getFittingWords pf pp word = (pp, map snd fittingPlacements)
  where
    possiblePlacements = getAllPossiblePlacements word pp
    fittingPlacements = filter (isWordFitting pf) possiblePlacements


-- check if the given word does not touch or overlap with conflicting pieces
isWordFitting :: PlayingField -> (MoveType, [PlacedPiece]) -> Bool
isWordFitting pf (_, [])   = True
isWordFitting pf (Horizontal, ((PlacedPiece c cs):pps)) =
  isFirstValid && (all (isEqualOrEmpty pf Horizontal) (init pps)) && isLastValid
  where
    -- Note: If the field is empty, then all three surrounding fields must be empty
    -- If the field is not empty, then only the field below it must be empty
    isFirstValid = case isFieldEmpty pf cs of
      False -> emptyNeighbors cs [L] pf
      True  -> emptyNeighbors cs [Up, Down, L] pf
    lastCoordinates = ppCoordinates (last pps)
    isLastValid = case isFieldEmpty pf lastCoordinates of
      False -> emptyNeighbors lastCoordinates [R] pf
      True  -> emptyNeighbors lastCoordinates [Up, Down, R] pf
isWordFitting pf (Vertical, ((PlacedPiece c cs):pps))   =
  isFirstValid && (all (isEqualOrEmpty pf Vertical) (init pps)) && isLastValid
  where
    isFirstValid = case isFieldEmpty pf cs of
      False -> emptyNeighbors cs [Down] pf
      True  -> emptyNeighbors cs [L, R, Down] pf
    lastCoordinates = ppCoordinates (last pps)
    isLastValid  = case isFieldEmpty pf lastCoordinates of
      False -> emptyNeighbors lastCoordinates [Up] pf
      True  -> emptyNeighbors lastCoordinates [L, R, Up] pf

isFieldEmpty :: PlayingField -> Coordinates -> Bool
isFieldEmpty pf cs = Map.notMember cs pf

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
emptyNeighbors cs ds pf = all (== True) $ map (isNeighborEmpty cs pf) ds


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
-- TODO: Allow words to be extended?
getAvailablePlacedPieces :: PlayingField -> [PlacedPiece]
getAvailablePlacedPieces pf = filter (not . (isPieceAvailable pf)) (Map.elems pf)

-- a piece is available if either its left and right or upper and lower
-- neigbors are free
isPieceAvailable :: PlayingField -> PlacedPiece -> Bool
isPieceAvailable pf (PlacedPiece c cs) = isVerticalFree || isHorizontalFree
  where
    neighbors = getNeighbors pf cs
    isVerticalFree   = all (== Nothing) [Map.lookup Up neighbors, Map.lookup Down neighbors]
    isHorizontalFree = all (== Nothing) [Map.lookup L neighbors, Map.lookup R neighbors]


executeTurn :: PlayingField -> Bag -> Maybe (PlayingField, Bag)
executeTurn pf []      = Nothing  -- end of the game
executeTurn pf (w:ws)  = case viablePlacementOptions w of
  [] -> executeTurn pf ws  -- try the next word in the bag
  -- we simply pick the first viable placement option for a word
  -- this could be tuned later
  ((pp, (pps:_)):xs) -> Just (insertPlacedPieces pps pf, ws)
  where
    -- all placed pieces where a word could be attached
    availablePlacedPieces = getAvailablePlacedPieces pf
    allPlacementOptions w = map (\pp -> getFittingWords pf pp w) availablePlacedPieces
    viablePlacementOptions w = filter (\(pp, pps) -> not (null pps)) (allPlacementOptions w)


insertPlacedPieces :: [PlacedPiece] -> PlayingField -> PlayingField
insertPlacedPieces [] pf = pf
insertPlacedPieces (pp@(PlacedPiece c cs):pps) pf = case Map.lookup cs pf of
  Nothing  -> insertPlacedPieces pps (Map.insert cs pp pf)
  Just pp' -> case pp == pp' of
    True  -> insertPlacedPieces pps (Map.insert cs pp pf)
    False -> error $ "The given piece " ++ show pp ++ " is invalid. " ++ show pp'


executeGame :: PlayingField -> Bag -> IO PlayingField
executeGame pf bag = executeGame' 1 pf bag


executeGame' :: Int -> PlayingField -> Bag -> IO PlayingField
executeGame' _ pf []  = return pf
executeGame' turn pf bag  = do
  let filePath = "/tmp/shcrabble_" ++ show turn ++ ".svg"
  writePlayingField filePath pf
  let maybeEndOfGame = executeTurn pf bag
  case maybeEndOfGame of
    Nothing          -> putStrLn "The End." >> return pf
    Just (pf', bag') -> executeGame' (turn + 1) pf' bag'


readWordFile :: FilePath -> IO [String]
readWordFile path = do
  content <- readFile path
  let messyWords = concatMap words $ lines content
      cleanWords = map (filter (`elem` allowedCharacters)) messyWords
      longerWords = filter (\x -> length x > 1) cleanWords
      -- Heuristic: We sort words from longest to shortest to maximize
      -- the number of fields that other words can attach to
      sortedWords = reverse $ sortBy (comparing length) longerWords
  return sortedWords


main :: IO ()
main = do
  args <- getArgs
  if length args == 0
    then error "Please provide a file with words. Usage: ./shcrabble \"wordFile.txt\"\n"
    else do
      allWords <- readWordFile (head args)
      let trie = foldl (flip insert) emptyTrie allWords
          (firstWord:remainingWords) = allWords
          playingField = placeFirstWord firstWord Map.empty
      -- putStrLn $ "A trie: " ++ show trie
      -- putStrLn $ "All prefixes: " ++ show (allPrefixes trie)
      -- putStrLn $ "The playing field: " ++ show playingField
      -- putStrLn $ generatePlayingFieldSVG playingField
      putStrLn $ "allWords: " ++ show allWords
      -- writePlayingField "/tmp/shcrabble.svg" playingField
      -- putStrLn $ "Free neighbors: " ++ show (getAllFreeNeighbors playingField)

      playingField' <- executeGame playingField remainingWords
      writePlayingField "/tmp/shcrabble_final.svg" playingField'
      -- putStrLn $ "remainingWords': " ++ show remainingWords'

