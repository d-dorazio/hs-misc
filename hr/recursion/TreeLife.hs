#!/usr/bin/env stack
-- stack --resolver lts-9.2 script
{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}

-- nice and super fun!

import Control.Applicative
import Control.Monad (forM_)
import Control.Monad.Trans.State

import Data.Binary (Word16)
import Data.Bits (Bits, shiftL, testBit, (.&.))
import Data.Char (isSpace)
import Data.Either (isRight)
import Data.Maybe (fromJust, fromMaybe)

import qualified Data.Map as Map


------------------------------------------------
-- Tree
------------------------------------------------
data Tree a = Branch !a !(Tree a) !(Tree a) | Leaf !a deriving (Eq, Show)
data TreeDirection = DLeft | DRight deriving (Eq, Show)

type Neighbors a = (Maybe a, Maybe a, a, Maybe a) -- parent, left child, current, right child

treeRootVal :: Tree a -> a
treeRootVal (Leaf v      ) = v
treeRootVal (Branch v _ _) = v

treeMapWithNeighbors :: (Neighbors a -> b) -> Tree a -> Tree b
treeMapWithNeighbors f = go Nothing
 where
  go parent (Leaf v) = Leaf $ f (parent, Nothing, v, Nothing)
  go parent (Branch v l r) =
    let v' = f (parent, childVal l, v, childVal r)
        l' = go (Just v) l
        r' = go (Just v) r
    in  Branch v' l' r'

  childVal = Just . treeRootVal

treeFocus :: [TreeDirection] -> Tree a -> Maybe (Tree a)
treeFocus [] t = Just t
treeFocus (d:ds) (Branch _ l r) =
  let st = case d of
        DLeft     -> l
        otherwise -> r
  in  treeFocus ds st
treeFocus _ _ = Nothing


------------------------------------------------
-- Rule
------------------------------------------------
-- parent, leftChild, current, rightChild
type RuleInput = (Cell, Cell, Cell, Cell)

newtype Rule = Rule (Map.Map RuleInput Cell) deriving (Eq, Show)

applyRule :: Rule -> RuleInput -> Cell
applyRule (Rule ruleMap) = (Map.!) ruleMap

decodeRule :: Word16 -> Rule
decodeRule rule = Rule ruleMap
 where
  ruleMap = foldl f Map.empty [15, 14 .. 0]

  f rMap bn = Map.insert (bitNToRuleInput bn) (bitAtToCell rule bn) rMap

bitNToRuleInput :: Bits a => a -> RuleInput
bitNToRuleInput bn = (bitAtToCell bn 3, bitAtToCell bn 2, bitAtToCell bn 1, bitAtToCell bn 0)

bitAtToCell :: Bits a => a -> Int -> Cell
bitAtToCell b n = if testBit b n then Alive else Dead


------------------------------------------------
-- CA
------------------------------------------------
data Cell = Alive | Dead deriving (Eq, Ord, Show)

type CA = Tree Cell

prettyCA :: CA -> String
prettyCA (Leaf c      ) = prettyCell c
prettyCA (Branch c l r) = '(' : prettyCA l ++ " " ++ prettyCell c ++ " " ++ prettyCA r ++ ")"

prettyCell :: Cell -> String
prettyCell Alive = "X"
prettyCell Dead  = "."

nextCA :: Rule -> CA -> CA
nextCA r = treeMapWithNeighbors (applyRule r . mkRuleInput)
 where
  mkRuleInput (mparent, mleftChild, current, mrightChild) =
    (unwrap mparent, unwrap mleftChild, current, unwrap mrightChild)
  unwrap = fromMaybe Dead


------------------------------------------------
-- Main
------------------------------------------------
main = do
  rule        <- fmap decodeRule readLn
  (Right ica) <- fmap (parse caParser) getLine
  nt          <- readLn

  runTest 0 rule nt (Map.singleton 0 ica)

runTest :: Int -> Rule -> Int -> Map.Map Int CA -> IO ()
runTest _            _    0      _   = return ()
runTest currentCAKey rule nTests cas = do
  (rnq:rpath:[]) <- fmap words getLine

  let nq           = read rnq
  let (Right path) = parse treeDirectionParser rpath

  let (ca', cas')  = generateCAs currentCAKey cas nq

  putStrLn . prettyCell . treeRootVal . fromJust . treeFocus path $ ca'

  runTest (currentCAKey + nq) rule (nTests - 1) cas'
 where
  generateCAs :: Int -> Map.Map Int CA -> Int -> (CA, Map.Map Int CA)
  generateCAs caKey allCas nQueries =
    let range = if nQueries >= 0
          then [caKey .. (caKey + nQueries)]
          else [(caKey - 1), (caKey - 2) .. (caKey + nQueries)]
        ca = (Map.!) allCas caKey
    in  foldl gen (ca, allCas) range

  gen :: (CA, Map.Map Int CA) -> Int -> (CA, Map.Map Int CA)
  gen (prevCa, allCas) i = case Map.lookup i allCas of
    Just ca -> (ca, allCas)
    Nothing -> let ca' = nextCA rule prevCa in (ca', Map.insert i ca' allCas)

------------------------------------------------
-- Parsers
------------------------------------------------
caParser :: Parser CA
caParser = many space >> tree
 where
  tree   = (Leaf <$> cell) <|> branch
  cell   = alive <|> dead
  alive  = lexeme 'X' >> return Alive
  dead   = lexeme '.' >> return Dead

  branch = parens $ do
    l <- tree
    v <- cell
    r <- tree
    return $ Branch v l r

treeDirectionParser :: Parser [TreeDirection]
treeDirectionParser = brackets $ many direction
 where
  direction = dleft <|> dright
  dleft     = char '<' >> return DLeft
  dright    = char '>' >> return DRight


------------------------------------------------
-- Parser Machinery
------------------------------------------------
newtype Parser a = Parser (String -> Either String (String, a))

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> fmap (\(r, a) -> (r, f a)) (p s)

instance Applicative Parser where
  pure v = Parser $ \s -> return (s, v)
  (Parser f) <*> (Parser p) = Parser $ \s -> do
    (s', f') <- f s
    (fs, v) <- p s'
    return (fs, f' v)

instance Monad Parser where
  fail msg = Parser $ \_ -> Left msg
  return = pure
  (Parser p) >>= f = Parser $ \s -> do
    (s', v) <- p s
    let (Parser p') = f v
    p' s'

instance Alternative Parser where
  -- uhm, is this right? e.g. does it respect the laws
  empty = fail "no more alternatives"
  (Parser p) <|> (Parser g) = Parser $ \s ->
    case p s of
      r | isRight r -> r
      otherwise -> g s

parse :: Parser a -> String -> Either String a
parse (Parser p) = fmap snd . p

parens :: Parser a -> Parser a
parens = between (lexeme '(') (lexeme ')')

brackets :: Parser a -> Parser a
brackets = between (lexeme '[') (lexeme ']')

between :: Parser a -> Parser b -> Parser c -> Parser c
between o c p = do
  _ <- o
  r <- p
  _ <- c
  return r

lexeme :: Char -> Parser Char
lexeme c = char c <* many space

char :: Char -> Parser Char
char c = satisfy (==c)

space :: Parser Char
space = satisfy isSpace

satisfy :: (Char -> Bool) -> Parser Char
satisfy pr = Parser $ \s -> case s of
  []            -> Left "empty input"
  (c:cs) | pr c -> return (cs, c)
  otherwise     -> Left "not satisfied"

end :: Parser ()
end = Parser $ \s -> if null s then return ([], ()) else fail "not reached the end"


------------------------------------------------
-- Tests
------------------------------------------------
testDecodeRule :: IO ()
testDecodeRule = testCases "decodeRule" testIt cases
 where
  cases =
    [ (0     , [])
    , (1     , [((Dead, Dead, Dead, Dead), Alive)])
    , (0xFFFF, map (\n -> (bitNToRuleInput n, Alive)) ([15, 14 .. 0] :: [Int]))
    ]
  testIt encodedRule =
    let (Rule r) = decodeRule encodedRule
    in  if Map.size r /= 16
          then Left "decodeRule's size is not 16(e.g. not all bits have been read)"
          else Right (filter (\(_, c) -> c == Alive) . Map.toList $ r)

testCaParser :: IO ()
testCaParser = testCases "caParser" (parse caParser) cases
 where
  cases =
    [ ("X"            , Leaf Alive)
    , ("."            , Leaf Dead)
    , ("(X . .)"      , Branch Dead (Leaf Alive) (Leaf Dead))
    , ("(X . (X . .))", Branch Dead (Leaf Alive) (Branch Dead (Leaf Alive) (Leaf Dead)))
    , ("((X . .) . X)", Branch Dead (Branch Dead (Leaf Alive) (Leaf Dead)) (Leaf Alive))
    ]

testTreeDirectionParser :: IO ()
testTreeDirectionParser = testCases "treeDirectionParser" (parse treeDirectionParser) cases
 where
  cases =
    [ ("[]"     , [])
    , ("[>]"    , [DRight])
    , ("[<]"    , [DLeft])
    , ("[<<>><]", [DLeft, DLeft, DRight, DRight, DLeft])
    ]

testCases :: (Eq b, Show a, Show b, Show e) => String -> (a -> Either e b) -> [(a, b)] -> IO ()
testCases title f cases = do
  putStrLn $ "*** Test " ++ title ++ " ***"

  forM_ cases $ \(inp, exp) -> do
    putStr $ "Test: " ++ (show inp) ++ " -> "
    case f inp of
      Left  err -> print err
      Right t   -> if t /= exp
        then putStrLn $ "expected " ++ (show exp) ++ " but got " ++ (show t) ++ " instead"
        else putStrLn "OK"
