module Gnosis.Tui where

import Control.Monad     (foldM, void)

import System.Console.ANSI
import System.Console.ANSI.Types
import System.IO
import System.Posix.Terminal
import System.Posix.IO

data Style
  = Style { margin    :: (Int, Int, Int, Int)
          , color     :: (ColorIntensity, Color)
          , bgColor   :: (ColorIntensity, Color) }
  deriving (Show)

data Component
  = Text   Style String
  | Button Style String
  | Link   Style String String
  | HGroup Style [Component]
  | VGroup Style [Component]
  deriving (Show)

data RenderState
  = RenderState { posX :: Int
                , posY :: Int
                , w :: Int
                , h :: Int }

type Render = RenderState -> Component -> IO RenderState

renderGroup :: Render -> RenderState -> Style -> [Component] -> IO RenderState
renderGroup render RenderState{ posX=posX, posY=posY } style children = do
  let Style{ margin  = (top, right, bottom, left)
           , color   = (colorIntensity, color)
           , bgColor = (bgColorIntensity, bgColor) } = style
  setSGR [SetColor Foreground colorIntensity color]
  setSGR [SetColor Background bgColorIntensity bgColor]

  let newState = RenderState { posX = posX + left
                             , posY = posY + top
                             , w = 0
                             , h = 0 }

  RenderState { posX = posX'
              , posY = posY'
              , w = w
              , h = h } <- foldM render newState children

  pure RenderState { posX = posX'
                   , posY = posY'
                   , w = left + right + w
                   , h = top + bottom + h}

renderComponent :: RenderState -> Component -> IO RenderState
renderComponent RenderState{ posX=posX, posY=posY } (Text style value) = do
  let Style{ margin  = (top, right, bottom, left)
           , color   = (colorIntensity, color)
           , bgColor = (bgColorIntensity, bgColor) } = style
  setSGR [SetColor Foreground colorIntensity color]
  setSGR [SetColor Background bgColorIntensity bgColor]

  setCursorPosition (posY + top) (posX + left)
  putStr value

  let (w, h) = (left + right + length value, top + bottom + 1)
  pure RenderState { posX = posX + w
                   , posY = posY + h
                   , w = w
                   , h = h }
renderComponent state (HGroup style children) = do
  renderGroup renderChildren state style children
  where
    renderChildren :: Render
    renderChildren state' el = do
      let RenderState{ posX=oldX, posY=oldY, w=oldW, h=oldH } = state'
      RenderState{ w=w, h=h } <- renderComponent state' el
      pure RenderState{ posX = oldX + w
                      , posY = oldY
                      , w = oldW + w
                      , h = max oldH h }
renderComponent state (VGroup style children) = do
  renderGroup renderChildren state style children
  where
    renderChildren :: Render
    renderChildren state' el = do
      let RenderState{ posX=oldX, posY=oldY, w=oldW, h=oldH } = state'
      RenderState{ w=w, h=h } <- renderComponent state' el
      pure RenderState{ posX = oldX
                      , posY = oldY + h
                      , w = max oldW w
                      , h = oldH + h }

enableRawMode :: IO TerminalAttributes
enableRawMode = do
  old <- getTerminalAttributes stdInput
  let raw = foldl withoutMode old
        [ EnableEcho
        , ProcessInput
        , KeyboardInterrupts
        , StartStopOutput ]
  setTerminalAttributes stdInput raw Immediately
  return old

restoreMode :: TerminalAttributes -> IO ()
restoreMode old = setTerminalAttributes stdInput old Immediately
