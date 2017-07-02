module Lib (
  insertPlacedPieces,
  optionsInfo,
  placeFirstWord,
  readWordFile,
  writePlayingField
) where

import SVG
import Types

import Data.Semigroup ((<>))
import Options.Applicative (Parser, ParserInfo, argument, fullDesc, header, help,
  helper, info, long, metavar, short, str, strOption, switch, (<**>))

import qualified Data.Map as Map

-- Parsing of command-line arguments

optionsParser :: Parser Options
optionsParser =
  Options
    <$> switch (long "generate-svg-per-turn" <> short 'g' <> help "Generate an svg for each turn.")
    <*> argument str (metavar "INPUT_FILE" <> help "File to read the wordlist from.")
    <*> argument str (metavar "OUTPUT_FILE" <> help "File to write the final SVG to.")

optionsInfo :: ParserInfo Options
optionsInfo = info parser description
  where
    parser = optionsParser <**> helper
    description = fullDesc <> header "Shcrabble - generate Scrabble-boards from any text file!"

-- we limit ourselves to only ASCII chars fow now
lowerAlphabet = ['a' .. 'z']
upperAlphabet = ['A' .. 'Z']
allowedCharacters = lowerAlphabet ++ upperAlphabet

readWordFile :: FilePath -> IO [String]
readWordFile path = do
  content <- readFile path
  let messyWords = concatMap words $ lines content
      cleanWords = map (filter (`elem` allowedCharacters)) messyWords
      longerWords = filter (\x -> length x > 1) cleanWords
  return longerWords

-- Game code

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


insertPlacedPieces :: [PlacedPiece] -> PlayingField -> PlayingField
insertPlacedPieces [] pf = pf
insertPlacedPieces (pp@(PlacedPiece c cs):pps) pf = case Map.lookup cs pf of
  Nothing  -> insertPlacedPieces pps (Map.insert cs pp pf)
  Just pp' -> case pp == pp' of
    True  -> insertPlacedPieces pps (Map.insert cs pp pf)
    False -> error $ "The given piece " ++ show pp ++ " is invalid. " ++ show pp'
