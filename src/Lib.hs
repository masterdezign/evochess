{-# LANGUAGE FlexibleInstances #-}

module Lib where

import Data.List
import Data.Array

data Kind = Rook | Knight | Bishop | Queen | King | Pawn
  deriving Eq

data Color = White | Black
  deriving Eq

type Square = Maybe Piece

data Piece = Piece Color Kind
  deriving Eq

type Coord = (Int, Int)

showP (Piece White Rook) = "R"
showP (Piece White Knight) = "N"
showP (Piece White Bishop) = "B"
showP (Piece White Queen) = "Q"
showP (Piece White King) = "K"
showP (Piece White Pawn) = "P"
showP (Piece Black Rook) = "r"
showP (Piece Black Knight) = "n"
showP (Piece Black Bishop) = "b"
showP (Piece Black Queen) = "q"
showP (Piece Black King) = "k"
showP (Piece Black Pawn) = "p"

showS Nothing = " "
showS (Just p) = showP p

instance Show Board where
  show (Board b) = concat $ intersperse "\n"s
    where s = [(intersperse ' ' [(head $ showS $ b ! (8 - i + 1, j)) | j <- [1..8]]) | i <- [1..8]]

newtype Board = Board { getBoard :: Array Coord Square }

boardBounds = ((1, 1), (8, 8))

inBoard :: Coord -> Bool
inBoard (x, y) = x > (fst $ fst boardBounds)
                 && x < (fst $ snd boardBounds)
                 && y < (snd $ fst boardBounds)
                 && y < (snd $ snd boardBounds)

-- Pieces in the very beginning of the game
board0 :: Board
board0 = Board $ listArray boardBounds b
  where
    b = bw ++ pw ++ b0 ++ b0 ++ b0 ++ b0 ++ pb ++ bb
    bw = map Just [Piece White Rook, Piece White Knight, Piece White Bishop, Piece White Queen
      , Piece White King, Piece White Bishop, Piece White Knight, Piece White Rook]
    bb = map Just [Piece Black Rook, Piece Black Knight, Piece Black Bishop, Piece Black Queen
      , Piece Black King, Piece Black Bishop, Piece Black Knight, Piece Black Rook]
    pw = replicate 8 (Just $ Piece White Pawn)
    pb = replicate 8 (Just $ Piece Black Pawn)
    b0 = replicate 8 Nothing

evoboard :: Board
evoboard = Board $ listArray boardBounds b
  where
    b = b1 ++ b2 ++ b0 ++ b0 ++ b0 ++ b0 ++ b7 ++ b8
    b0 = replicate 8 Nothing
    b1 = [Nothing, Nothing, Nothing, Nothing, Just $ Piece White King, Nothing, Nothing, Nothing]
    b2 = replicate 8 (Just $ Piece White Pawn)  -- White pawns only
    b7 = replicate 8 (Just $ Piece Black Pawn)
    b8 = [Nothing, Nothing, Nothing, Nothing, Just $ Piece Black King, Nothing, Nothing, Nothing]

emptyBoard :: Board
emptyBoard = Board $ listArray boardBounds (repeat Nothing)

isValidMove :: Board -> Coord -> Coord -> Bool
isValidMove _ _ _ = False

move :: Board -> Coord -> Coord -> Board
move _ _ _ = board0
