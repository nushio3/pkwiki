module Solver where

data Problem = Problem 
     { wisdomFlag :: Bool,
       x1 :: Double,
       x2 :: Double,
       y1 :: Double,
       y2 :: Double,
       probSize :: Int,
       probRepeat :: Int
     }

type Answer = (Double, Double)

type Solver = Problem -> IO Answer

