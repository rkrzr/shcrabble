module Lib (
  readWordFile
) where

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

