module Main where

import Lib as L
import Data.Maybe
import Control.Monad

getAnswer rawFirst rawSecond = do
  firstHand <- parseHand rawFirst
  secondHand <- parseHand rawSecond
  return $ compare firstHand secondHand

loop = do
  putStrLn "First hand plz"
  rawFirst <- getLine
  putStrLn "Second hand plz"
  rawSecond <- getLine
  let answer = getAnswer rawFirst rawSecond
  if isJust answer
    then putStrLn $ "First hand was " ++ (show (fromJust answer)) ++ " second hand"
    else putStrLn "parse error"


main :: IO ()
main = do
  forever loop
