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

import Control.Monad (when)
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
  let maybeEndOfGame = placeWords bag pf
  case maybeEndOfGame of
    Nothing          -> pure pf
    Just (pf', bag') -> executeGame' os bag' pf' (turn+1)


placeWords :: T.Bag -> T.PlayingField -> Maybe (T.PlayingField, T.Bag)
placeWords [] pf = Nothing  -- no words left in the bag
placeWords ws pf = case matchingWords of
    []  -> Nothing  -- there were no more matching words
    -- w:remainingWords -> placeWords remainingWords (placeWord w pf)
    w:remainingWords -> Just (placeWord w pf, remainingWords)
  where
    -- we simply skip words until there is one that matches
    matchingWords = dropWhile noMatch ws
    noMatch w = all null $ concatMap (L.getFittingWords pf w) (Map.elems pf)


placeWord :: String -> T.PlayingField -> T.PlayingField
placeWord word pf = case possiblePlacements of
    []  -> error $ "It is not possible to place the word: " ++ word
    -- we arbitrarily pick the first possible placement
    pps:_ -> L.insertPlacedPieces pps pf
  where
    possiblePlacements = concatMap (L.getFittingWords pf word) (Map.elems pf)


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
