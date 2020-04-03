module Bowling
where

type Throw = Int
type Score = Int

score :: [Throw] -> Score
score throws = min 300 (sum throws) 


