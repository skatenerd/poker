{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Lib as L
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Data.Either

playHand :: EitherT String IO Ordering
playHand = do
  lift $ putStrLn "First hand plz"
  rawFirst <- lift getLine
  firstHand <- either left right $ parseHand rawFirst
  lift $ putStrLn "Second hand plz"
  rawSecond <- lift getLine
  secondHand <- either left right $ parseHand rawSecond
  right $ compare firstHand secondHand

presentAnswer answer = let message = "\n*******************\nFirst hand was " ++ (show answer) ++ " second hand\n*******************"
                       in (putStrLn message) >> (putStrLn "NEXT HAND\n")

loop = do
  result :: (Either String Ordering) <- runEitherT playHand
  either putStrLn presentAnswer result
  return ()

main :: IO ()
main = forever loop
