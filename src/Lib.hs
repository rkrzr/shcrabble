module Lib (
  someFunc,
  emptyTrie,
  insertChar,
  insertWord
) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Trie = Trie {tChar :: Char, isWord :: Bool, tChildren :: Map.Map Char Trie}
  deriving (Eq, Ord, Show)

-- The space is a hack. It stands for the "empty character" here.
emptyTrie isEndOfWord = Trie ' ' isEndOfWord Map.empty

insertChar :: Char -> Bool -> Trie -> Trie
insertChar c1 isEndOfWord trie@(Trie c2 w children)
  -- return the unchanged trie if the char was already in there
    | Map.member c1 children == True && (isWord $ fromJust $ Map.lookup c1 children) == isEndOfWord = trie
  -- update isEndOfWord
    | Map.member c1 children == True  = Trie c2 w updatedChildren
  -- otherwise add the char with an empty trie as a value
    | otherwise = Trie c2 w (Map.insert c1 (emptyTrie isEndOfWord) children)
  where
    Trie _ _ children2 = fromJust $ Map.lookup c1 children
    updatedChildTrie = (Trie c1 True children2)
    updatedChildren = Map.insert c1 updatedChildTrie children

insertWord :: String -> Trie -> Trie
insertWord [] trie = trie
-- the last char must be marked as the end of the word
insertWord [x] trie = insertChar x True trie
insertWord (x:xs) trie@(Trie c w children) = Trie c w newerMap
  where
    Trie _ _ newMap = insertChar x False trie
    childTrie = fromJust $ Map.lookup x newMap
    newerMap = Map.insert x (insertWord xs childTrie) newMap
