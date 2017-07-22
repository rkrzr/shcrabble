-- The streaming version of shcrabble:
-- Given a text-file as input this program will attempt to build a shcrabble
-- board from the words in the file in the order that they appear there.
-- It will place the first word in the center of the playing board and link
-- all subsequent words to the earlier words.
-- If a word cannot be placed it will simply be skipped.

module Main where

import qualified Lib as L
import qualified Types as T

import qualified Data.Map.Strict as Map

import Control.Monad (when)
import Debug.Trace (trace, traceShowId)
import Options.Applicative (execParser)
import System.Directory (doesFileExist)


-- TODO: Don't copy this code
executeGame :: T.Options -> T.Bag -> T.PlayingField -> IO T.PlayingField
executeGame os bag pf = executeGame' os bag pf 1


executeGame' :: T.Options -> T.Bag -> T.PlayingField -> Int -> IO T.PlayingField
executeGame' _  []  pf _    = return pf
executeGame' os bag pf turn = do
  let filePath = (T.oSvgFile os) ++ show turn ++ ".svg"
  -- write an SVG file for each turn if requested
  _ <- when (T.oGenerateSvgPerTurn os) (L.writePlayingField filePath pf)

  print $ "Remaining bag: " ++ (show bag)
  let maybeEndOfGame = placeWord bag pf
  print $ "Ordered playing field: " ++ (show $ Map.toAscList pf)
  case maybeEndOfGame of
    Nothing          -> pure pf
    Just (pf', bag') -> executeGame' os bag' pf' (turn+1)


-- Place a word, as long as there are any fitting words left
placeWord :: T.Bag -> T.PlayingField -> Maybe (T.PlayingField, T.Bag)
placeWord [] pf = Nothing  -- no words left in the bag
placeWord (w:remainingWords) pf = Just (placeWord' w pf, remainingWords)


placeWord' :: String -> T.PlayingField -> T.PlayingField
placeWord' word pf = case possiblePlacements of
    []  -> pf  -- just skip a word, if we cannot place it anywhere
    -- we arbitrarily pick the first possible placement
    pps:_ -> L.insertPlacedPieces pps pf
  where
    -- walk pf pieces from closest to the center to furthest from the center
    orderedPlacedPieces = map snd (Map.toAscList pf)
    -- and pick the first word that matches
    possiblePlacements = concatMap (L.getFittingWords pf word)  orderedPlacedPieces


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
      finalPlayingField <- executeGame options remainingWords playingField

      L.writePlayingField outputFilename finalPlayingField
    else do
      putStrLn ("Word file does not exist: " ++ filePath)
