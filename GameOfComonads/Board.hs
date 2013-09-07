module GameOfComonads.Board where

import Control.Arrow ((***))
import Control.Applicative ((<*>))
import Control.Comonad

newtype Board a = Board { unBoard :: [[a]] }

instance Functor Board where
    fmap f (Board b) = Board $ fmap (fmap f) b

data Pointer a = Pointer { pointerPos :: (Int, Int), pointerBoard :: Board a }

instance Functor Pointer where
    fmap f p@(Pointer { pointerBoard = b }) = p { pointerBoard = fmap f b }

instance Comonad Pointer where
    extract (Pointer { pointerPos = (x, y), pointerBoard = Board b }) =
        b !! x !! y

    duplicate p@(Pointer { pointerPos = pos, pointerBoard = Board b }) =
        p { pointerBoard = newBoard }
            where newBoard = Board $ updateBoard b [0..length b]
                  updateBoard = zipWith (\row x -> updateRow x row [0..length row])
                  updateRow x = zipWith (\cell y -> p { pointerPos = (x, y) })

pointerNeighbours :: Pointer a -> [a]
pointerNeighbours p@(Pointer { pointerPos = (x, y), pointerBoard = Board board }) =
    fmap (\pos -> extract $ p { pointerPos = pos }) positions
        where inBounds (a, b) = a > 0 && b > 0 && a < length board && b < length (board !! a)
              offsets = ((filter (/= (0, 0)) . concat) .) . zipWith (zip . repeat) <*> repeat $ [-1..1]
              positions = filter inBounds $ fmap ((+ x) *** (+ y)) offsets

liveNeighbours :: Pointer Bool -> Int
liveNeighbours = length . filter id . pointerNeighbours

rules :: Pointer Bool -> Bool
rules p | c && (n < 2 || n > 3) = False
        | (c && n == 2) || n == 3 = True
        | otherwise = c
      where c = extract p
            n = liveNeighbours p

step :: Board Bool -> Board Bool
step = pointerBoard . extend rules . Pointer (0, 0)
