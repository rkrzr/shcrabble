module Main where

import SVG
import Trie
import Types
import qualified Data.Map as Map


someWords :: [String]
someWords = ["just", "some", "random", "words", "without", "meaning"]

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

-- check if the given word does not touch or intersect with other pieces
isWordFitting :: PlacedPiece -> [PlacedPiece] -> PlayingField -> Bool
isWordFitting pp pps pf = undefined

-- It's often possible to attach a word in several ways to a given piece
-- think e.g. about words that contain the character of the placed piece several times
getAllPossiblePlacements :: String -> PlacedPiece -> [[PlacedPiece]]
getAllPossiblePlacements word pp = undefined

placeWord :: String -> PlacedPiece -> PlayingField -> PlayingField
placeWord = undefined

removeWord :: String -> Bag -> Bag
removeWord = undefined

-- get all placed pieces where a word could be attached, i.e.
-- all pieces where either the x or the y-axis is still free
-- TODO: Allow words to be extended?
getAvailablePlacedPieces :: PlayingField -> [PlacedPiece]
getAvailablePlacedPieces pf = Map.elems pf  -- TODO: Implement this properly

executeTurn :: PlayingField -> Bag -> (PlayingField, Bag)
executeTurn pf []  = (pf, [])  -- the game should end here
executeTurn pf bag = case matchingWords of
    []           -> error "No more words can be played."
    ((_, []): _) -> error "Should not happen. We filter this out earlier."
    ((pp, (word:words)):ws) -> (placeWord word pp pf, removeWord word bag)
  where
    -- all placed pieces where a word could be attached
    availablePlacedPieces = getAvailablePlacedPieces pf

    matchingWords :: [(PlacedPiece, [String])]
    matchingWords = filter (\(_, ws) -> ws /= []) $ map (getMatchingWords bag) availablePlacedPieces
    -- _fittingWords = filter isWordFitting matchingWords

main :: IO ()
main = do
  let trie = foldl (flip insert) emptyTrie someWords
      firstWord = head someWords
      _remainingWords = tail someWords
      playingField = placeFirstWord firstWord Map.empty
  putStrLn $ "A trie: " ++ show trie
  putStrLn $ "All prefixes: " ++ show (allPrefixes trie)
  putStrLn $ "The playing field: " ++ show playingField
  -- putStrLn $ generatePlayingFieldSVG playingField
  writePlayingField "/tmp/shcrabble.svg" playingField
  -- putStrLn $ "Free neighbors: " ++ show (getAllFreeNeighbors playingField)

  -- let (playingField', remainingWords') = executeTurn playingField remainingWords
  -- writePlayingField "/tmp/shcrabble2.svg" playingField'
  -- putStrLn $ "remainingWords': " ++ show remainingWords'

