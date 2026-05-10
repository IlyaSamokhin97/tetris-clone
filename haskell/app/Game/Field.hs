module Game.Field
  ( Cell (..)
  , Pos (..), (+.)
  , Field , mkField, (!?)
  , dims
  , copyIf
  , rotate
  ) where

import GHC.Arr
import GHC.Ix

data Cell
  = Empty
  | Falling
  | Frozen
  | Disappearing
  deriving (Eq, Show)

-- newtype needed to show array index error with values
-- default instance for pairs doesn't do that
newtype Pos
  = Pos (Int, Int)
  deriving (Eq, Ord, Show)

instance Ix Pos where
  range (Pos b, Pos e) =
    map Pos $ range (b, e)

  index (Pos b, Pos e) (Pos i)
    | inRange (b, e) i = unsafeIndex (b, e) i
    | otherwise        = GHC.Ix.indexError (b, e) i "Pos"

  inRange (Pos b, Pos e) (Pos i) =
    inRange (b, e) i

-- same fixity as +
infixl 6 +.
(+.) :: Pos -> Pos -> Pos
Pos (ay, ax) +. Pos (by, bx) = Pos $ (ay + by, ax + bx)

type Field = Array Pos Cell

-- same fixity as !
infixr 8 !?
(!?) :: Ix i => Array i a -> i -> Maybe a
a !? i = if inRange (bounds a) i then Just (a ! i) else Nothing

mkField :: Field
mkField = array bounds_ [(i, Empty) | i <- range bounds_]
  where
    width = 10
    height = 20
    bounds_ = (Pos (0, 0), Pos (height-1, width-1))

dims :: Array Pos Cell -> (Int, Int)
dims a = (y' - y + 1, x' - x + 1)
  where (Pos (y, x), Pos (y', x')) = bounds a

copyIf :: (Cell -> Bool) -> Field -> (Int, Int) -> (Int, Int) -> Field -> Field
copyIf cond src (y0, x0) (y1', x1') dst =
  dst // [(Pos (y', x'), src ! Pos (y, x)) | (y, y') <- zip [0..] [y0..y1]
                                           , (x, x') <- zip [0..] [x0..x1]
                                           , cond $ src ! Pos (y, x)
                                           ]
  where
    (h, w) = dims dst
    y1 = min (y0 + y1' - 1) (h - 1)
    x1 = min (x0 + x1' - 1) (w - 1)

rotate :: Bool -> Field -> Field
rotate left field
  | y0 == x0 && y1 == x1 = array (bounds field) [(rotateIx i, field ! i) | i <- indices field]
  | otherwise            = error "Field must be square"
  where
    (Pos (y0, x0), Pos (y1, x1)) = bounds field
    (h, w) = dims field

    rotateIx (Pos (y, x)) = Pos $ if left
      then (w - 1 - x,         y)
      else (        x, h - 1 - y)
    
