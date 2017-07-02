module Lib (
  optionsInfo,
  readWordFile
) where

import Types

import Data.Semigroup ((<>))
import Options.Applicative (Parser, ParserInfo, argument, fullDesc, header, help,
  helper, info, long, metavar, short, str, strOption, switch, (<**>))

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

