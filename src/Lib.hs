{- This is a board-centric approach which guarantees that
 - each piece has unique coordinates.
 -
 - The game stores the following information:
 - 1. Board
 - 2. Turn number. Odd numbers correspond to the moves of White,
 -    even numbers, of Black.
 - 3. Number of pawn moves (including captures by pawns), required
 -    for a pawn-minor piece transformation. Both players.
 - 4. Number of piece captures, required for rook promotion. Both
 -    players.
 - 5. Is en passant defined? If yes, then the information abouth
 -    the last long pawn move has to be stored.
 - 6. Location of both kings to quickly check for pins.
 -    Alternatively, no kings location is needed if every enemy's
 -    piece is checked if the king is in its attack range.
 -
 - Information about castling is not stored since it is
 - not defined for the evochess.
 -
 -}
{-# LANGUAGE FlexibleInstances #-}

module Lib where

import Data.List
import Data.Array
import Data.Maybe ( fromJust )

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

pawnDirection :: Color -> Int
pawnDirection White = 1
pawnDirection Black = -1

-- Verify if a piece given by coord is under attack.
-- Especially useful to see if a king is in check.
-- That is needed to verify if a piece is pinned.
-- A piece is pinned if after its move the king of
-- the same color would be in check.
-- Thus, the game should not only retain the board
-- information, but also it should be able to quickly
-- find the position of both kings.
isAttacked :: Board -> Coord -> Bool
isAttacked board@(Board brd) coord@(x, y) = pawn1 || pawn2 || other
  where
    -- Using fromJust as we are sure the piece exists
    color = getColor $ fromJust $ getSquare board coord

    -- Check if there are any enemy pawns
    pawn1 = pawnAttacks (x - 1, y - opdirection)
    pawn2 = pawnAttacks (x + 1, y - opdirection)

    pawnAttacks coord' = (getSquare board coord') == (Just $ Piece opcolor Pawn)
    opcolor = opposite color
    opdirection = pawnDirection opcolor

    other = False

getSquare :: Board -> Coord -> Square
getSquare (Board brd) coord | not (inBoard coord) = Nothing
                            | otherwise = brd ! coord

getColor :: Piece -> Color
getColor (Piece White _) = White
getColor _ = Black

opposite :: Color -> Color
opposite White = Black
opposite _ = White

isValidMove :: Board -> Coord -> Coord -> Bool
isValidMove _ _ _ = False

move :: Board -> Coord -> Coord -> Board
move _ _ _ = board0
