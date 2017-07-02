-- The streaming version of shcrabble:
-- Given a text-file as input this program will attempt to build a shcrabble
-- board from the words in the file in the order that they appear there.
-- It will place the first word in the center of the playing board and link
-- all subsequent words to the earlier words.
-- If a word cannot be placed it will simply be skipped.

module Main where

import qualified Lib as L
import qualified Types as T

import Options.Applicative (execParser)
import System.Directory (doesFileExist)

main :: IO ()
main = do
  options <- execParser L.optionsInfo
  let filePath = T.oWordFile options
  fileExists <- doesFileExist filePath
  if fileExists
    then do
      allWords <- L.readWordFile filePath
      mapM_ print allWords
    else do
      putStrLn ("Word file does not exist: " ++ filePath)
