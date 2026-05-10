module Game.Timer
  ( Timer , mkTimer
  , tick
  ) where

data Timer = Timer
  { timespan :: Double
  , current  :: Double
  } deriving Show

mkTimer :: Double -> Timer
mkTimer x = Timer
  { timespan = x
  , current  = x
  }

tick :: Double -> Timer -> (Timer, Bool)
tick dt t@Timer { timespan, current } =
  if current - dt < 0.0
  then (t { current = timespan     }, True )
  else (t { current = current - dt }, False)
