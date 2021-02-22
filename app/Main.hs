module Main where

import Foobar

main :: IO ()
main = do
  results <- foobar "cue.txt"

  mapM_ (putStrLn . show) results
