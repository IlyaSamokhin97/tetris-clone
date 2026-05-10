module Game.Draw
  ( field
  , clear
  ) where

import Control.Monad
import Data.Array.Storable
import Data.Bifunctor

import Canvas
import Game.Field

type Point = (Int, Int)

field :: Field -> Point -> Int -> Bool -> Canvas -> IO ()
field fld (y0, x0) cellPx showDisappearing canvas = do
  drawBorder
  drawCells
  where
    drawBorder :: IO ()
    drawBorder = forM_ borders $ \i -> writeArray canvas i white
      where
        borders      = [(y, x) | y <- [y0..y1], x <- [x0..x1], isBorder y x]
        isBorder y x = y `elem` [y0, y1] || x `elem` [x0, x1]
        (y1, x1)     = (h `cellsFrom` y0, w `cellsFrom` x0)
        (h, w)       = dims fld

    drawCells :: IO ()
    drawCells =
      forM_ drawableCells $ \(Pos (y, x), color) ->
        cell color (y `cellsFrom` y0, x `cellsFrom` x0) cellPx canvas
      where
        drawableCells = map (second cellColor) . filter isDrawable $ assocs fld

        isDrawable (_, c) = case c of
          Falling      -> True
          Frozen       -> True
          Disappearing -> showDisappearing
          Empty        -> False

        cellColor Falling = blue
        cellColor _       = white

    cells `cellsFrom` x = x + cells * cellPx

cell :: Pixel -> Point -> Int -> Canvas -> IO ()
cell color (y, x) cellPx canvas =
  forM_ (zip [cellPx, cellPx - 2, cellPx - 8] $ cycle [black, color]) $ \(px, c) ->
    fillSquare c (y + cellPx - px, x + cellPx - px) (y + px - 1, x + px - 1) canvas

fillSquare :: Pixel -> Point -> Point -> Canvas -> IO ()
fillSquare color (y0, x0) (y1, x1) canvas =
  forM_ [(y, x) | y <- [y0..y1], x <- [x0..x1]] $ \i ->
    writeArray canvas i color

clear :: Canvas -> IO ()
clear canvas = do
  (b, e) <- getBounds canvas
  fillSquare black b e canvas
