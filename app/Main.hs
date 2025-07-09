module Main where

import Control.Exception (bracket)

import System.Console.ANSI
import System.Console.ANSI.Types
import System.IO

import Gnosis.Tui
import Gnosis.Ssh (start)

component :: Component
component = HGroup Style{ margin  = (1, 0, 0, 1)
                        , color   = (Vivid, Red)
                        , bgColor = (Vivid, Blue)}
            [ VGroup Style{ margin  = (0, 1, 0, 0)
                          , color   = (Vivid, Magenta)
                          , bgColor = (Vivid, Blue)}
              [ Text Style{ margin  = (0, 1, 0, 0)
                          , color   = (Vivid, Blue)
                          , bgColor = (Vivid, Magenta)}
                "world hello 1"
              , Text Style{ margin  = (1, 0, 0, 0)
                          , color   = (Vivid, Blue)
                          , bgColor = (Vivid, Magenta)}
                "world hello 2"]
            , Text Style{ margin  = (1, 0, 0, 0)
                        , color   = (Vivid, Magenta)
                        , bgColor = (Vivid, White)}
              "-"
            , VGroup Style{ margin  = (0, 0, 0, 11)
                          , color   = (Vivid, Red)
                          , bgColor = (Vivid, Blue)}
              [ Text Style{ margin  = (0, 0, 0, 1)
                          , color   = (Vivid, Red)
                          , bgColor = (Vivid, Blue)}
                "hello world 3"]]

main :: IO ()
main = do
    stdoutSupportsANSI <- hNowSupportsANSI stdout
    if not stdoutSupportsANSI
    then
        putStrLn "Standard output does not support 'ANSI' escape codes."
    else do
        bracket enableRawMode restoreMode $ \_ -> do
          clearScreen
          RenderState{ h=h } <- renderComponent RenderState{ posX=0, posY=0, w=0, h=0 } component
          setSGR [Reset]
          setCursorPosition (1 + h) 0
          setSGR [SetColor Foreground Vivid Green]
          putStr "Press keys, q to quit"
          setSGR [Reset]
          hFlush stdout
          loop (5 + h)
          pure ()

loop :: Int -> IO ()
loop wasd = do
  c <- getChar
  setCursorPosition wasd 0
  clearLine
  setSGR [SetColor Foreground Vivid Yellow]
  putStr ("You pressed: " ++ [c])
  setSGR [Reset]
  hFlush stdout
  if c == 'q'
  then do
    setCursorPosition (5 + wasd) 0
    putStrLn "Exiting."
  else loop wasd
