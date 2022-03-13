module Main where

import Control.Concurrent ( threadDelay )
import Data.Either (isRight, fromRight, isLeft)
import Control.Exception (assert)
import Graphics.Ueberzug
    ( clear,
      defaultUbConf,
      draw,
      newUeberzug,
      Scalers(FitContain),
      UbConf(identifier, path, x, y, width, height, scaler) )

forceRight :: Either a b -> b
forceRight (Right x) = x

forceLeft :: Either a b -> a
forceLeft (Left x) = x

main :: IO ()
main = do
  testDrawThenClear
  testEmptyPathFails
  testEmptyIdenFails

testDrawThenClear :: IO ()
testDrawThenClear = do
  let ub = newUeberzug
  e_ub <-
    draw ub $ defaultUbConf
      { identifier = "75933779_p0"
      -- relative to repo root
      , path = "test/75933779_p0.jpg"
      , x = 10
      , y = 2
      , width = Just 10
      , height = Just 10
      , scaler = Just FitContain
      }
  let ub = assert (isRight e_ub) $ forceRight e_ub
  threadDelay 1000000

  e_ub <- clear ub "75933779_p0"
  let ub = assert (isRight e_ub) $ forceRight e_ub
  threadDelay 1000000

testEmptyPathFails :: IO ()
testEmptyPathFails = do
  let ub = newUeberzug
  e_ub <-
    draw ub $ defaultUbConf
      { identifier = "75933779_p0"
      , path = ""
      }
  let msg = assert (isLeft e_ub) $ forceLeft e_ub
  assertEq msg "Incomplete Information : Path Not Found"
  threadDelay 1000000

testEmptyIdenFails :: IO ()
testEmptyIdenFails = do
  let ub = newUeberzug
  e_ub <-
    draw ub $ defaultUbConf
      { identifier = ""
      }
  let msg = assert (isLeft e_ub) $ forceLeft e_ub
  assertEq msg "Incomplete Information : Identifier Not Found"
  threadDelay 1000000

-- | This is in the IO monad to force evaluation
assertEq :: String -> String -> IO ()
assertEq x value =
  if x == value
     then pure ()
     else error $ "Expected " <> show value <> " but got " <> show x
