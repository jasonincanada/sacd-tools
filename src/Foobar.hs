{- Foobar.hs - convert a UDSACD files to foobar format -}

module Foobar where

import Data.Function    ((&))
import Data.List.Split  (chunksOf, splitOn)
import Text.Printf      (printf)


{- Operations -}

foobar :: FilePath -> IO [Foobar]
foobar file = do
  cue <- readFile file

  cue & lines                     -- split file into lines
      & map (dropWhile (==' '))   -- trim leading spaces
      & map words                 -- split lines into words
      & map parse                 -- convert each to its Line object
      & filter useful             -- keep only the ones we want
      & process                   -- turn cue objects into foobar objects
      & return 

  where
    useful :: Line -> Bool
    useful Other = False
    useful _     = True


{- Types -}

-- a line from cue.txt is either a track number identifier, an index,
-- or something else that we can ignore
data Line  = Track
           | Index Int Time
           | Other
             deriving Show

data Foobar = Foobar Time Time  -- gap, length
              deriving Show

data Time   = Time Int Int Int -- min, sec, frame
              deriving Eq

instance Show Time where
  show (Time m s f) = printf "%02d%02d%02d" m s f


{- Parsing -}

parse :: [String] -> Line
parse ("TRACK" : _             ) = Track
parse ("INDEX" : num : time : _) = Index (read num) (toTime time)
parse _                          = Other

toTime :: String -> Time
toTime s = Time min sec from
  where
    (min : sec : from : _) = splitOn ":" s & map read


{- Processing -}

-- group the list of line objects into chunks of 3. note this assumes a very
-- specific ordering of objects, namely repetitions of TRACK,INDEX,INDEX
-- so this needs to be already filtered to TRACK/INDEX lines only
--
-- the zipWith/tail trick pairs each element of a list with the one after it
-- and doesn't make a pair with only the last element--just what we want
process :: [Line] -> [Foobar]
process lines = zipWith convert triples (tail triples)
  where
    triples = chunksOf 3 lines


-- convert a pair of tracks to a foobar line (gap/length)
convert :: [Line] -> [Line] -> Foobar
convert a b = Foobar gap len
  where
    gap = time2 `sub` time1
    len = time3 `sub` time2

    (Track  : Index 0 time1  : Index 1 time2 : _) = a
    (Track  : Index 0 time3  : Index 1 _     : _) = b


-- custom subtraction since the frame is base 75
sub :: Time -> Time -> Time
sub (Time min1 sec1 frame1) (Time min2 sec2 frame2)
    = Time (min1-min2) (sec1-sec2) (frame1-frame2)
        & fix

      where
        fix = fixSec . fixFrame

        fixFrame (Time m s f)
          | f < 0     = Time m (s-1) (f+75)
          | otherwise = Time m  s     f

        fixSec   (Time m s f)
          | s < 0     = Time (m-1) (s+60) f
          | otherwise = Time  m     s     f

        -- if we ever implement add, these are the overflow clauses
        --
        -- | f >= 75   = Time m (s+1) (f-75)
        -- | s >= 60   = Time (m+1) (s-60) f

