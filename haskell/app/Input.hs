module Input
  ( Input, mkInput, mouse, keyboard
  , Mouse, MouseKey(..)
  , Keyboard, KbKey(..)
  , Button, mkButton, update, isPressed, justPressed
  ) where

import Data.Map.Strict (Map, fromList)

data Input = Input
  { mouse    :: Mouse
  , keyboard :: Keyboard
  } deriving (Show)

mkInput :: Input
mkInput = Input
  { mouse = mkMouse
  , keyboard = mkKeyboard
  }

type Mouse = Map MouseKey Button

data MouseKey
  = MouseLeft
  | MouseRight
  deriving (Enum, Bounded, Eq, Ord, Show)

mkMouse :: Mouse
mkMouse = fromList $ map (, mkButton) ([minBound..maxBound] :: [MouseKey])

type Keyboard = Map KbKey Button

data KbKey
  = KbLeft
  | KbRight
  | KbDown
  deriving (Enum, Bounded, Eq, Ord, Show)

mkKeyboard :: Keyboard
mkKeyboard = fromList $ map (, mkButton) ([minBound..maxBound] :: [KbKey])

data Button = Button
  { prev :: Bool
  , curr :: Bool
  }

instance Show Button where
  show Button { curr, prev } = map toChar [curr, prev]
    where toChar b = if b then '_' else 'T'

mkButton :: Button
mkButton = Button
  { prev = False
  , curr = False
  }

update :: Bool -> Button -> Button
update curr' old = Button
  { prev = curr old
  , curr = curr'
  }

isPressed :: Button -> Bool
isPressed = curr

justPressed :: Button -> Bool
justPressed = \case
  Button { prev = False, curr = True } -> True
  _                                    -> False
