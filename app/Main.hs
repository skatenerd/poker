{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Lib as L
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Data.Either

parseHandE s = let parsed = parseHand s
               in if isJust parsed
                 then right parsed
                 else left "Parse Error"

playHand :: EitherT String IO ()
playHand = do
  lift $ putStrLn "First hand plz"
  rawFirst <- lift getLine
  firstHand <- parseHandE rawFirst
  lift $ putStrLn "Second hand plz"
  rawSecond <- lift getLine
  secondHand <- parseHandE rawSecond
  let answer = compare firstHand secondHand
  lift $ putStrLn $ "\n*******************\nFirst hand was " ++ (show answer) ++ " second hand\n*******************"
  lift $ putStrLn "\nNEXT HAND\n"

loop = do
  result :: (Either String ()) <- runEitherT playHand
  either putStrLn (const $ return ()) result
  return ()

main :: IO ()
main = forever loop
