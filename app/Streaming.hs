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
placeWords []     pf = pf
placeWords (w:ws) pf = case matchingWords of
    []  -> error "There were no more matching words."
    w:_ -> placeWords ws (placeWord w pf)
  where
    matchingWords = getMatchingWords ws


placeWord :: String -> T.PlayingField -> T.PlayingField
placeWord word pf = case possiblePlacements of
    []  -> error $ "It is not possible to place the word: " ++ word
    -- we arbitrarily pick the first possible placement
    p:_ -> insertWord p pf
  where
    possiblePlacements = getPlacements word pf


getMatchingWords = undefined
getPlacements = undefined
insertWord = undefined


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
