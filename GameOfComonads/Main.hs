module Main (main) where

import Control.Comonad
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomIO)
import UI.NCurses

import GameOfComonads.Board

generateBoard :: Integer -> Integer -> IO (Board Bool)
generateBoard w h = fmap Board $ sequence [ sequence [ randomIO | _ <- [0..h] ] | _ <- [0..w] ]

loop :: Window -> Board Bool -> Curses ()
loop w b = do
  updateWindow w $ drawBoard b
  render
  maybeEvent <- getEvent w (Just 0)
  unless (maybeEvent == Just (EventCharacter 'q')) . loop w $ step b

drawBoard :: Board Bool -> Update ()
drawBoard = sequence_ . concat . unBoard . pointerBoard . extend drawCell . Pointer (0, 0)

drawCell p@(Pointer { pointerPos = (x, y) }) = do
  moveCursor (fromIntegral x) (fromIntegral y)
  drawString $ if extract p then "#" else " "

main :: IO ()
main = do
  (w, h) <- runCurses screenSize

  -- Curses has a problem drawing near edges - had to take 2 off size.
  board <- generateBoard (w - 2) (h - 2)

  runCurses $ do
         setEcho False
         w <- defaultWindow
         color <- newColorID ColorCyan ColorBlack 1
         updateWindow w $ setColor color
         loop w board
