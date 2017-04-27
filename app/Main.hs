module Main where

import Trie


main :: IO ()
main = do
  let trie = insert "asd" (insert "asdf" emptyTrie)
  putStrLn $ "A trie: " ++ show trie
  putStrLn $ "All prefixes: " ++ show (allPrefixes trie)
