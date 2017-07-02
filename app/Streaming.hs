-- The streaming version of shcrabble:
-- Given a text-file as input this program will attempt to build a shcrabble
-- board from the words in the file in the order that they appear there.
-- It will place the first word in the center of the playing board and link
-- all subsequent words to the earlier words.
-- If a word cannot be placed it will simply be skipped.

module Main where

import qualified Lib as L
import qualified Types as T

import qualified Data.Map as Map

import Options.Applicative (execParser)
import System.Directory (doesFileExist)


placeWords :: [String] -> T.PlayingField -> T.PlayingField
placeWords [] pf = pf
placeWords ws pf = case matchingWords of
    []  -> error "There were no more matching words."
    w:remainingWords -> placeWords remainingWords (placeWord w pf)
  where
    -- drop words until there is one that matches
    matchingWords = dropWhile (\w -> getMatches w pf == []) ws


placeWord :: String -> T.PlayingField -> T.PlayingField
placeWord word pf = case possiblePlacements of
    []  -> error $ "It is not possible to place the word: " ++ word
    -- we arbitrarily pick the first possible placement
    pps:_ -> L.insertPlacedPieces pps pf
  where
    possiblePlacements = getPlacements word pf


getMatches :: String -> T.PlayingField -> [T.PlacedPiece]
getMatches = _

getPlacements = _


main :: IO ()
main = do
  options <- execParser L.optionsInfo
  let filePath = T.oWordFile options
  fileExists <- doesFileExist filePath
  if fileExists
    then do
      allWords <- L.readWordFile filePath
      -- mapM_ print allWords
      let (firstWord:remainingWords) = allWords
          playingField = L.placeFirstWord firstWord Map.empty
          outputFilename = (T.oSvgFile options) ++ ".svg"
          finalPlayingField = placeWords remainingWords playingField

      L.writePlayingField outputFilename playingField
    else do
      putStrLn ("Word file does not exist: " ++ filePath)
