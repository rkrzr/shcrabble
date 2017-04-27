module Main where

import SVG
import Trie
import Types
import qualified Data.Map as Map


someWords :: [String]
someWords = ["just", "some", "random", "words", "without", "meaning"]


placeFirstWord' :: String -> PlayingField -> Coordinates -> PlayingField
placeFirstWord' [] pf _         = pf
placeFirstWord' (z:zs) pf (x,y) = placeFirstWord' zs newPlayingField (x+1, y)
  where newPlayingField = Map.insert (x,y) z pf

-- we always place the first word starting from (1,1) to the right
placeFirstWord :: String -> PlayingField -> PlayingField
placeFirstWord [] pf = pf
placeFirstWord xs pf = placeFirstWord' xs pf (1,1)

writePlayingField :: FilePath -> PlayingField -> IO ()
writePlayingField filePath pf = writeFile filePath (generatePlayingFieldSVG pf)


main :: IO ()
main = do
  let trie = foldl (flip insert) emptyTrie someWords
      firstWord = head someWords
      playingField = placeFirstWord firstWord Map.empty
  putStrLn $ "A trie: " ++ show trie
  putStrLn $ "All prefixes: " ++ show (allPrefixes trie)
  putStrLn $ "The playing field: " ++ show playingField
  putStrLn $ generatePlayingFieldSVG playingField
  writePlayingField "/tmp/shcrabble.svg" playingField
