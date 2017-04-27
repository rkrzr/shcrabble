module Main where

-- import Lib
import Trie
-- import qualified Data.Map as Map


main :: IO ()
main = do
  -- let trie = upsertChar emptyTrie2 ('a', True) Map.empty
  --     trie2 = upsertChar trie ('b', True) Map.empty
  let trie = insert "asd" (insert "asdf" emptyTrie)
  putStrLn $ "A trie: " ++ show trie
  putStrLn $ "All prefixes: " ++ show (allPrefixes trie)
