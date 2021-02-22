module Main where

import Cue

main :: IO ()
main = do
  results <- foobar "cue.txt"

  mapM_ (putStrLn . show) results
