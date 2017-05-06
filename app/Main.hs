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


someWords :: [String]
someWords = ["just", "some", "random", "words", "without", "meaning"]

lowerAlphabet = ['a' .. 'z']
upperAlphabet = ['A' .. 'Z']
allowedCharacters = lowerAlphabet ++ upperAlphabet

allDirections :: [Direction]
allDirections = [L ..]

placeFirstWord' :: String -> PlayingField -> Coordinates -> PlayingField
placeFirstWord' [] pf _         = pf
placeFirstWord' (z:zs) pf cs@(x,y) = placeFirstWord' zs newPlayingField (x+1, y)
  where newPlayingField = Map.insert cs (PlacedPiece z cs) pf

-- we always place the first word starting from (1,1) to the right
placeFirstWord :: String -> PlayingField -> PlayingField
placeFirstWord [] pf = pf
placeFirstWord xs pf = placeFirstWord' xs pf (1,1)

writePlayingField :: FilePath -> PlayingField -> IO ()
writePlayingField filePath pf = writeFile filePath (generatePlayingFieldSVG pf)

getNeighbors :: PlayingField -> Coordinates -> Map.Map Direction (Maybe PlacedPiece)
getNeighbors pf (x,y) = Map.map (\cs -> Map.lookup cs pf) neighborMap
  where
    neighborCoordinates = [(L, (x-1, y)), (R, (x+1, y)), (Up, (x, y+1)), (Down, (x, y-1))]
    neighborMap = Map.fromList neighborCoordinates

-- getFreeNeighbors :: PlayingField -> Coordinates -> Map
-- getFreeNeighbors

-- getFreeNeighbors :: PlayingField -> Coordinates -> [(Coordinates, Bool)]
-- getFreeNeighbors pf (x,y) = neighbors
--   where
--     neighborCoordinates = [(x-1, y), (x+1, y), (x, y+1), (x, y-1)]
--     isFieldFree cs = Map.notMember cs pf
--     neighbors = map (\(d, cs) -> (cs, isFieldFree cs)) neighborCoordinates


-- getAllFreeNeighbors :: PlayingField -> [(Coordinates, Bool)]
-- getAllFreeNeighbors pf = concatMap (getFreeNeighbors pf) (Map.keys pf)

-- find all words in the bag that could be attached to the given character
getMatchingWords :: Bag -> PlacedPiece -> (PlacedPiece, [String])
getMatchingWords [] pp                   = (pp, [])
getMatchingWords xs pp@(PlacedPiece c _) = (pp, filter (elem c) xs)

-- Note: We are generating all theoretical options here, they may not be possible
-- in practice, if there are already different pieces on the playing field
getHorizontalPlacements' :: String -> String -> PlacedPiece -> [[PlacedPiece]]
getHorizontalPlacements' prefix [] pp@(PlacedPiece c cs) = []
getHorizontalPlacements' prefix (x:xs) pp@(PlacedPiece c cs) = case x == c of
  -- skip characters that don't match
  False -> getHorizontalPlacements' (prefix ++ [x]) xs pp
  -- place the prefix to the left and the suffix to the right of (x,y)
  True  -> let pps = placeString L prefix pp ++ [pp] ++ placeString R xs pp
           in pps : getHorizontalPlacements' (prefix ++ [x]) xs pp

getHorizontalPlacements :: String -> PlayingField -> PlacedPiece -> [[PlacedPiece]]
getHorizontalPlacements word pf pp = getHorizontalPlacements' [] word pp

-- we place words either from left to right or from top to bottom
getPlacement :: String -> PlacedPiece -> Char -> [PlacedPiece]
getPlacement word pp char = undefined

-- For the given word, return *all* possible placements given the contraints of the playing field
getFittingWords :: PlayingField -> PlacedPiece -> String -> (PlacedPiece, [PlacedPiece])
getFittingWords pf pp [] = (pp, [])
getFittingWords pf pp word = (pp, possiblePlacements)
  where
    possiblePlacements = concatMap (getPlacement word pp) word

-- check if the given word does not touch or intersect with other pieces
isWordFitting :: PlayingField -> PlacedPiece -> [PlacedPiece] -> Bool
isWordFitting pf pp pps = undefined

-- It's often possible to attach a word in several ways to a given piece
-- think e.g. about words that contain the character of the placed piece several times
getAllPossiblePlacements :: String -> PlacedPiece -> [[PlacedPiece]]
getAllPossiblePlacements word pp = undefined

-- Note: We only allow *either* a vertical *or* a horizontal move for now
determineFreeDirection :: PlacedPiece -> PlayingField -> Maybe MoveType
determineFreeDirection (PlacedPiece c cs) pf = if isVerticalFree
    then Just Vertical
    else if isHorizontalFree
      then Just Horizontal
      else Nothing
  where
    neighbors = getNeighbors pf cs
    verticalNeighbors   = map (\d -> fromJust (Map.lookup d neighbors)) [Up, Down]
    horizontalNeighbors = map (\d -> fromJust (Map.lookup d neighbors)) [L, R]
    isVerticalFree   = all (== Nothing) verticalNeighbors
    isHorizontalFree = all (== Nothing) horizontalNeighbors

placeWord :: String -> PlacedPiece -> PlayingField -> PlayingField
placeWord [] pp pf = pf
placeWord word pp pf = case determineFreeDirection pp pf of
    Just Horizontal -> insertWord Horizontal word pp pf
    Just Vertical   -> insertWord Vertical word pp pf
    -- TODO: Don't use trace here.
    -- TODO: Retry the skipped word later?
    Nothing         -> trace ("Skipping the word \"" ++ word ++ "\". There is no free spot.") pf

goOne :: Direction -> Coordinates -> Coordinates
goOne Up   (x,y) = (x, y+1)
goOne Down (x,y) = (x, y-1)
goOne L    (x,y) = (x-1, y)
goOne R    (x,y) = (x+1, y)

-- TODO: Use this function in insertString
placeString :: Direction -> String -> PlacedPiece -> [PlacedPiece]
placeString _ [] _ = []
placeString d (w:ws) (PlacedPiece c cs) = pp' : placeString d ws pp'
  where
    cs' = goOne d cs
    pp' = PlacedPiece w cs'

-- insert a string in the given direction starting from the given placed piece
insertString :: Direction -> String -> PlacedPiece -> PlayingField -> PlayingField
insertString _ []     _                  pf = pf
insertString d (w:ws) (PlacedPiece c cs) pf = insertString d ws pp' pf'
  where
    cs' = goOne d cs
    pp' = PlacedPiece w cs'
    pf' = Map.insert cs' pp' pf

-- insert a word at or around the given placed piece
-- Note: we assume here that we validated earlier that the word fits
insertWord :: MoveType -> String -> PlacedPiece -> PlayingField -> PlayingField
insertWord mt word pp@(PlacedPiece c cs) pf =
  let
    (prefix, _c:suffix) = break (== c) word
  in case mt of
    Horizontal -> insertString R suffix pp (insertString L (reverse prefix) pp pf)
    -- Note: SVG spans coordindates to the right and down...
    Vertical   -> insertString Up suffix pp (insertString Down (reverse prefix) pp pf)

removeWord :: String -> Bag -> Bag
removeWord _ []   = error "Cannot remove a word from an empty bag."
removeWord word bag = delete word bag

-- get all placed pieces where a word could be attached, i.e.
-- all pieces where either the x or the y-axis is still free
-- TODO: Allow words to be extended?
getAvailablePlacedPieces :: PlayingField -> [PlacedPiece]
getAvailablePlacedPieces pf = Map.elems pf  -- TODO: Implement this properly

executeTurn :: PlayingField -> Bag -> Maybe (PlayingField, Bag)
executeTurn pf []  = Nothing  -- end of the game
executeTurn pf bag = case matchingWords of
    []           -> trace "No more words can be played." Nothing
    ((_, []): _) -> error "Should not happen. We filter this out earlier."
    ((pp, (word:words)):ws) -> Just (placeWord (traceShowId word) pp pf, removeWord word bag)
  where
    -- all placed pieces where a word could be attached
    availablePlacedPieces = getAvailablePlacedPieces pf

    matchingWords :: [(PlacedPiece, [String])]
    matchingWords = filter (\(_, ws) -> ws /= []) $ map (getMatchingWords bag) availablePlacedPieces
    -- fittingWords = filter (isWordFitting pf) matchingWords

executeGame :: PlayingField -> Bag -> IO PlayingField
executeGame pf []  = return pf
executeGame pf bag = do
  let maybeEndOfGame = executeTurn pf bag
  case maybeEndOfGame of
    Nothing          -> putStrLn "The End." >> return pf
    Just (pf', bag') -> executeGame pf' bag'

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
      writePlayingField "/tmp/shcrabble.svg" playingField
      -- putStrLn $ "Free neighbors: " ++ show (getAllFreeNeighbors playingField)

      -- let (playingField', remainingWords') = executeTurn playingField remainingWords
      playingField' <- executeGame playingField remainingWords
      writePlayingField "/tmp/shcrabble2.svg" playingField'
      -- putStrLn $ "remainingWords': " ++ show remainingWords'

