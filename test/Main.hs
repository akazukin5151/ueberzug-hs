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

guard True _    = pure ()
guard False msg = error msg

forceLeft :: Either a b -> a
forceLeft (Left x) = x

main :: IO ()
main = do
  testDrawThenClear
  testTwoImagesAndClear
  testEmptyPathFails
  testEmptyIdenFails

testDrawThenClear :: IO ()
testDrawThenClear = do
  ub <- newUeberzug
  ei <-
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
  guard (isRight ei) "ei is Left but expected Right"
  threadDelay 1000000

  clear ub "75933779_p0"
  threadDelay 1000000

testTwoImagesAndClear :: IO ()
testTwoImagesAndClear = do
  ub <- newUeberzug
  ei1 <-
    draw ub $ defaultUbConf
      { identifier = "75933779_p0_0"
      , path = "test/75933779_p0.jpg"
      , x = 10
      , y = 2
      , width = Just 10
      , height = Just 10
      , scaler = Just FitContain
      }
  guard (isRight ei1) "ei1 is Left but expected Right"
  threadDelay 1000000

  ei2 <-
    draw ub $ defaultUbConf
      { identifier = "75933779_p0_1"
      , path = "test/75933779_p0.jpg"
      , x = 20
      , y = 2
      , width = Just 10
      , height = Just 10
      , scaler = Just FitContain
      }
  guard (isRight ei2) "ei2 is Left but expected Right"
  threadDelay 1000000

  clear ub "75933779_p0_0"
  threadDelay 1000000
  clear ub "75933779_p0_1"
  threadDelay 1000000

testEmptyPathFails :: IO ()
testEmptyPathFails = do
  ub <- newUeberzug
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
  ub <- newUeberzug
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
