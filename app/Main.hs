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
getMatchingWords :: Char -> Bag -> (Char, [String])
getMatchingWords c [] = (c, [])
getMatchingWords c xs = (c, filter (elem c) xs)

executeTurn :: PlayingField -> Bag -> (PlayingField, Bag)
executeTurn pf [] = (pf, [])  -- the game should end here
executeTurn _pf _words = undefined
  where
    _freeCharacters = undefined
    _matchingWords = map getMatchingWords _freeCharacters


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

