module Main where

import Control.Exception (bracket)

import System.Console.ANSI
import System.IO
import System.Posix.Terminal
import System.Posix.IO

import Gnosis.Ssh (start)

-- Set colors and write some text in those colors.
-- main :: IO ()
-- main = do
--     stdoutSupportsANSI <- hNowSupportsANSI stdout
--     if not stdoutSupportsANSI
--     then
--         putStrLn "Standard output does not support 'ANSI' escape codes."
--     else do
--         bracket enableRawMode restoreMode $ \_ -> do
--           clearScreen
--           setCursorPosition 5 10
--           setSGR [SetColor Foreground Vivid Green]
--           putStr "Press keys, q to quit"
--           setSGR [Reset]
--           hFlush stdout
--           loop

main :: IO ()
main = start

loop :: IO ()
loop = do
  c <- getChar
  setCursorPosition 7 10
  clearLine
  setSGR [SetColor Foreground Vivid Yellow]
  putStr ("You pressed: " ++ [c])
  setSGR [Reset]
  hFlush stdout
  if c == 'q'
  then do
    setCursorPosition 9 10
    putStrLn "Exiting."
  else loop

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
