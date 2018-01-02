module ParticleSwarm (
    solve
  , Pos3d
  , Particle (..)
  , pos3dAdd
  , particleMove
  , particleDist
  , particleParser
  , pos3dP
) where

import Control.Applicative (many)
import Control.Monad (void)

import Data.Function (on)
import Data.List (sortBy)

import qualified Data.Map as M

import Lib (Part(..), Parser, parseOrPrettyErr, lexeme, symbol, signedDecimal, angles)


-- brute force ftw :)
solve :: Part -> String -> String
solve part = parseOrPrettyErr (many particleParser) $ \particles ->
  let ipar = zip [0 ..] particles
  in  case part of
        Part1 ->
          let simulated = foldr (\_ -> moveAll) ipar ([1 .. 5000] :: [Int])
          in  (show :: Int -> String)
              . fst
              . head
              . sortBy (compare `on` (particleDist . snd))
              $ simulated
        Part2 ->
          let simulated = foldr (\_ -> removeCollided . moveAll) ipar ([1 .. 1000] :: [Int])
          in  show . length $ simulated
 where
  moveAll = map (\(i, p) -> (i, particleMove p))

  removeCollided parts = map head . M.elems . M.filter ((== 1) . length) $ particlesMap
   where
    particlesMap =
      foldr (\(i, p) -> M.alter (insertParticle (i, p)) (_particlePos p)) M.empty parts

    insertParticle p Nothing   = Just [p]
    insertParticle p (Just ps) = Just (p : ps)


type Pos3d = (Int, Int, Int)
data Particle = Particle
    { _particlePos :: Pos3d
    , _particleV :: Pos3d
    , _particleA :: Pos3d
    } deriving (Show)

pos3dAdd :: Pos3d -> Pos3d -> Pos3d
pos3dAdd (x, y, z) (x', y', z') = (x + x', y + y', z + z')

particleMove :: Particle -> Particle
particleMove (Particle p v a) = Particle p' v' a
 where
  p' = pos3dAdd p v'
  v' = pos3dAdd v a

particleDist :: Particle -> Int
particleDist (Particle (x, y, z) _ _) = abs x + abs y + abs z

--------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------

particleParser :: Parser Particle
particleParser = do
  p <- particleFieldP "p"
  comaP
  v <- particleFieldP "v"
  comaP
  a <- particleFieldP "a"
  return $ Particle p v a
 where
  particleFieldP pref = do
    _ <- symbol pref
    _ <- symbol "="
    angles (pos3dP)

pos3dP :: Parser Pos3d
pos3dP = do
  one <- signedDecimal
  comaP
  two <- signedDecimal
  comaP
  three <- signedDecimal
  return (one, two, three)

comaP :: Parser ()
comaP = void . lexeme . symbol $ ","
