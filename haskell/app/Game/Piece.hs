module Game.Piece
  ( Piece, mkPiece
  , PieceType (..)
  , dims
  , rowsCols
  , blueprint
  , turn
  , next
  ) where

import Data.Bifunctor
import Data.Bits ((.&.))
import Data.List (findIndex , sortOn)
import Data.List.Split
import Data.Maybe
import Data.Word

import Game.Field hiding (dims)
import Game.Field qualified as Field (dims)
import Game.Input

data Piece = Piece PieceType Rotation
  deriving Show

data Rotation
  = Deg0
  | Deg90
  | Deg180
  | Deg270
  deriving (Show, Eq, Ord, Enum, Bounded)

data PieceType
  = Square
  | Stick
  | L
  | ReverseL
  | T
  | S
  | ReverseS
  deriving (Show, Eq, Ord, Enum, Bounded)

mkPiece :: Piece
mkPiece = Piece Square Deg0

next :: Piece -> Piece
next (Piece ty _) = Piece (if ty < maxBound then succ ty else minBound) Deg0

turn :: Turn -> Piece -> Piece
turn t (Piece ty rot) = Piece ty $ case t of
  TurnLeft  -> if minBound < rot then pred rot else maxBound
  TurnRight -> if rot < maxBound then succ rot else minBound

dims :: Piece -> (Int, Int)
dims p =
  ( maybe h (h-) . findIndex (elem Falling) . reverse $ rows
  , maybe w (w-) . findIndex (elem Falling) . reverse $ cols
  )
  where
    b      = blueprint p
    (h, w) = Field.dims b
    (rows, cols) = rowsCols b

rowsCols :: Array Pos Cell -> ([[Cell]], [[Cell]])
rowsCols a =
  ( chunksOf w . map snd                 $ assocs a
  , chunksOf h . map snd . sortOn column $ assocs a
  )
  where
    (h, w) = Field.dims a
    column (Pos (_, x), _) = x

blueprint :: Piece -> Array Pos Cell
blueprint (Piece ty rot) =
  array (bounds rotated)
    [(i, fromMaybe Empty $ rotated !? (i +. src)) | i <- indices rotated]
  where 
    src       = Pos $ bimap fallingIx fallingIx $ rowsCols rotated
    fallingIx = fromMaybe 0 . findIndex (elem Falling)
    rotated   = iterate (rotate False) bp !! fromEnum rot

    bp = mkBlueprint $ case ty of
      Square   -> [ 0b_1100
                  , 0b_1100
                  , 0b_0000
                  , 0b_0000
                  ]
      Stick    -> [ 0b_1111
                  , 0b_0000
                  , 0b_0000
                  , 0b_0000
                  ]
      L        -> [ 0b_1000
                  , 0b_1000
                  , 0b_1100
                  , 0b_0000
                  ]
      ReverseL -> [ 0b_0100
                  , 0b_0100
                  , 0b_1100
                  , 0b_0000
                  ]
      T        -> [ 0b_0100
                  , 0b_1110
                  , 0b_0000
                  , 0b_0000
                  ]
      S        -> [ 0b_0110
                  , 0b_1100
                  , 0b_0000
                  , 0b_0000
                  ]
      ReverseS -> [ 0b_1100
                  , 0b_0110
                  , 0b_0000
                  , 0b_0000
                  ]

    mkBlueprint :: [Word8] -> Array Pos Cell
    mkBlueprint bs = listArray (Pos (0, 0), Pos (3, 3)) $ concatMap fromBin bs

    fromBin bin = map (fromBool . (/= 0) . (.&. bin)) [ 0b_1000
                                                      , 0b_0100
                                                      , 0b_0010
                                                      , 0b_0001
                                                      ]
    fromBool = \case
      True  -> Falling
      False -> Empty
    
