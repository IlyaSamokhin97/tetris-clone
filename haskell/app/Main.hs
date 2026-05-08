{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (lookup)

import Control.Monad
import GHC.Float
import Data.Array.Storable
import Data.ByteString (ByteString, pack)
import Data.Map.Strict (Map, empty, insertWith, traverseWithKey, lookup)
import Data.Maybe

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr

import Raylib.Core (clearBackground, initWindow, setTargetFPS, windowShouldClose, closeWindow, getFrameTime, isKeyDown, isMouseButtonDown)
import Raylib.Core.Textures
import Raylib.Core.Text (drawText)
import Raylib.Types
  ( Texture
  , Image (..)
  , PixelFormat (..)
  , KeyboardKey (KeyLeft, KeyRight, KeyDown)
  , MouseButton (MouseButtonLeft, MouseButtonRight)
  )
import Raylib.Util (drawing, raylibApplication, WindowResources)
import Raylib.Util.Colors (lightGray, rayWhite, black, white)

import Game qualified

import Canvas (Canvas)
import Canvas qualified

import Input hiding (update)
import Input qualified (update)

data Data = Data
  { canvas        :: Canvas
  , width         :: Int
  , height        :: Int
  , gameData      :: Game.Data
  , input         :: Input
  , texture       :: Texture
  }

mkData :: (Int, Int) -> IO Data
mkData (w, h) = do
  canvas <- newArray ((0, 0), (h - 1, w - 1)) Canvas.blue
  texture <- loadTextureFromImage Image
    { image'data = take (4 * w * h) . repeat $ 0
    , image'width = w
    , image'height = h
    , image'mipmaps = 1
    , image'format = PixelFormatUncompressedR8G8B8A8
    }
  pure Data
    { canvas        = canvas 
    , width         = w
    , height        = h
    , gameData      = Game.mkData
    , input         = mkInput
    , texture       = texture
    }

update :: Float -> Data -> IO Data
update dt d = do
  input' <- updateInput d
  (gameData', canvas') <- Game.update (gameData d) (canvas d) input' (float2Double dt)
  pure d
    { canvas = canvas'
    , gameData = gameData'
    , input = input'
    }
  where
    updateInput :: Data -> IO Input
    updateInput d = do
      k <- traverseWithKey (updateKey isKbKeyDown   ) (keyboard $ input d)
      m <- traverseWithKey (updateKey isMouseKeyDown) (mouse    $ input d)
      return (input d)
        { keyboard = k 
        , mouse    = m
        }

    updateKey :: (a -> IO Bool) -> a -> Button -> IO Button 
    updateKey isDown key prev = do
      curr <- isDown key
      return $ Input.update curr prev

    isKbKeyDown :: KbKey -> IO Bool
    isKbKeyDown key = isKeyDown $ case key of
      KbLeft -> KeyLeft
      KbRight -> KeyRight
      KbDown -> KeyDown

    isMouseKeyDown :: MouseKey -> IO Bool
    isMouseKeyDown key = isMouseButtonDown $ case key of
      MouseLeft -> MouseButtonLeft
      MouseRight -> MouseButtonRight

startup :: IO Data
startup = do
  let w = 1280
      h = 720
  _ <- initWindow w h "title"
  setTargetFPS 60
  mkData (w, h)

mainLoop :: Data -> IO Data
mainLoop oldD = do
  dt <- getFrameTime
  d <- update dt oldD
  withStorableArray (canvas d) $ \ptr ->
    updateTexture (texture d) (castPtr ptr)

  drawing $ do
    drawTexture (texture d) 0 0 white
  return d

shouldClose :: Data -> IO Bool
shouldClose _ = windowShouldClose

teardown :: Data -> IO ()
teardown _ = closeWindow Nothing

raylibApplication 'startup 'mainLoop 'shouldClose 'teardown
