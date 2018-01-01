module Main where

import Lib
import Types

import Control.Monad (when)
import Data.List (delete, sortBy)
import Data.Ord (comparing)
import Options.Applicative (execParser)
import System.Directory (doesFileExist)

import qualified Data.Map as Map

executeTurn :: PlayingField -> Bag -> Maybe (PlayingField, Bag)
executeTurn pf [] = Nothing -- end of the game
executeTurn pf ws =
  case sortedPlacements of
    [] -> Nothing
    ((w, pps):_) -> Just (insertPlacedPieces pps pf, delete w ws)
    -- all placed pieces where a word could be attached
  where
    availablePlacedPieces = getAvailablePlacedPieces pf
    wordsAndPieces = [(w, pp) | w <- ws, pp <- availablePlacedPieces]
    allPlacementOptions = concatMap (\(w, pp) -> getAllPlacementOptions w pf pp) wordsAndPieces
    viablePlacementOptions = filter (\(w, pps) -> not (null pps)) allPlacementOptions
    -- find the placement the closest to the center of the playing field
    sortedPlacements =
      sortBy
        (\(w1, pps1) (w2, pps2) -> comparing avgDistanceToMiddle pps1 pps2)
        viablePlacementOptions

executeGame :: Options -> PlayingField -> Bag -> IO PlayingField
executeGame os pf bag = executeGame' os pf bag 1

executeGame' :: Options -> PlayingField -> Bag -> Int -> IO PlayingField
executeGame' _ pf [] _ = return pf
executeGame' os pf bag turn = do
  let filePath = (oSvgFile os) ++ show turn ++ ".svg"
  -- write an SVG file for each turn if requested
  _ <- when (oGenerateSvgPerTurn os) (writePlayingField filePath pf)
  let maybeEndOfGame = executeTurn pf bag
  case maybeEndOfGame of
    Nothing -> putStrLn "The End." >> return pf
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
