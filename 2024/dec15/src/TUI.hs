{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module TUI where

import Brick
import Graphics.Vty
import Lens.Micro.Mtl (use, (.=))
import Lens.Micro.TH (makeLenses)

class State s where
  next :: s -> s
  advance :: s -> s
  done :: s -> Bool
  draw :: s -> [Widget ()]

data AppState a = AppState
  { _state :: a,
    _history :: [a]
  }
  deriving (Show, Eq)

makeLenses ''AppState

draw' :: (State a) => AppState a -> [Widget ()]
draw' (AppState {_state = s}) = draw s

chooseCursor :: AppState a -> [CursorLocation ()] -> Maybe (CursorLocation ())
chooseCursor _ _ = Nothing

handleEvent :: (State a) => BrickEvent () e -> EventM () (AppState a) ()
handleEvent (VtyEvent (EvKey (KChar ' ') _)) = do
  s <- use state
  if done s
    then halt
    else do
      state .= advance s
      history .= []
handleEvent (VtyEvent (EvKey KRight _)) = do
  s <- use state
  h <- use history
  if done s
    then halt
    else do
      let s' = advance s
      state .= s'
      history .= (s : h)
handleEvent (VtyEvent (EvKey KLeft _)) = do
  h <- use history
  case h of
    [] -> return ()
    (prev : h') -> do
      state .= prev
      history .= h'
handleEvent (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt
handleEvent _ = return ()

app :: (State a) => App (AppState a) e ()
app =
  App
    { appDraw = draw',
      appChooseCursor = chooseCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return (),
      appAttrMap = const $ attrMap defAttr []
    }

run :: (State a) => a -> IO a
run s = do
  let initial = AppState s []
  final <- defaultMain app initial
  return $ _state final
