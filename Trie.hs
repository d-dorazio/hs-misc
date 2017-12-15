#!/usr/bin/env stack
-- stack --resolver lts-9.0 script --package containers

import qualified Data.Map as Map

------------------------------------------------------------
-- Trie
------------------------------------------------------------

data CanExit = CanExit | CannotExit deriving (Eq, Show)

data Trie k = Trie CanExit (Map.Map k (Trie k)) deriving (Eq, Show)


buildTrie :: Foldable t => t a -> Trie a
buildTrie = foldr fun (Trie CanExit Map.empty)
 where
  fun :: a -> Trie a -> Trie a
  fun k v = Trie CannotExit $ Map.singleton k v

mergeTrie :: (Ord a) => Trie a -> Trie a -> Trie a
mergeTrie (Trie ce children) (Trie ce' children') =
  let canExit = case (ce, ce') of
        (CannotExit, CannotExit) -> CannotExit
        otherwise                -> CanExit
  in  Trie canExit $ Map.unionWith mergeTrie children children'

match :: Trie Char -> String -> Bool
match (Trie CanExit _       ) ""       = True
match (Trie _       children) (c:chrs) = case Map.lookup c children of
  Just subTrie -> match subTrie chrs
  Nothing      -> False

------------------------------------------------------------
-- Automata
------------------------------------------------------------
stringsToTrie :: [String] -> Trie Char
stringsToTrie = foldl (\trie s -> mergeTrie trie (buildTrie s)) $ Trie CanExit Map.empty

------------------------------------------------------------
-- Main
------------------------------------------------------------
main :: IO ()
main = do
  let strings = ["aa", "a"]
  let trie    = stringsToTrie strings
  print trie
  mapM_ (\s -> print s >> print (match trie s)) strings

  let t = buildTrie $ "a"
  print t
  print . match t $ "a"
