{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Prelude hiding (lookup)

import GHC.Float
import Data.Array.Storable
import Data.Map.Strict (traverseWithKey)

import Foreign.Ptr

import Raylib.Core
import Raylib.Core.Textures
import Raylib.Types
import Raylib.Util
import Raylib.Util.Colors

import Game qualified

import Canvas (Canvas)
import Canvas qualified

import Input hiding (update)
import Input qualified (update)

data Data = Data
  { canvas   :: Canvas
  , width    :: Int
  , height   :: Int
  , gameData :: Game.Data
  , input    :: Input
  , texture  :: Texture
  }

mkData :: (Int, Int) -> IO Data
mkData (w, h) = do
  canvas <- newArray ((0, 0), (h - 1, w - 1)) Canvas.blue
  texture <- loadTextureFromImage Image
    { image'data = take (4 * w * h) (repeat 0)
    , image'width = w
    , image'height = h
    , image'mipmaps = 1
    , image'format = PixelFormatUncompressedR8G8B8A8
    }
  pure Data
    { canvas   = canvas 
    , width    = w
    , height   = h
    , gameData = Game.mkData
    , input    = mkInput
    , texture  = texture
    }

startup :: IO Data
startup = do
  let w = 1280
      h = 720
  _ <- initWindow w h "title"
  setTargetFPS 60
  mkData (w, h)

update :: Float -> Data -> IO Data
update dt d = do
  input' <- updateInput (input d)
  (gameData', canvas') <- Game.update (gameData d) (canvas d) input' (float2Double dt)
  pure d
    { canvas = canvas'
    , gameData = gameData'
    , input = input'
    }
  where
    updateInput :: Input -> IO Input
    updateInput input = do
      k <- traverseWithKey (updateKey isKbKeyDown   ) (keyboard input)
      m <- traverseWithKey (updateKey isMouseKeyDown) (mouse    input)
      return input
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

