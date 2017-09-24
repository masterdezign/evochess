{-  The EvoChess Game
    Copyright (C) Bogdan Penkovsky 2017

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

 Evochess
 ========

 == Rules

 See rules.txt

 == Implementation

 This is a board-centric approach which guarantees that
 - each piece has unique coordinates.

 The game stores the following information:
 1. Board
 2. Turn number. Odd numbers correspond to the moves of White,
    even numbers, of Black.
 3. Number of Pawn moves (including captures by Pawns), required
    for a Pawn-minor piece transformation (#1).
 4. Number of piece captures, required for rook promotion (#2).
 5. The information about the last long Pawn move
    (the en passant rule).
 6. Location of both kings to quickly check for pins.
    Alternatively, no kings location is needed when every opponent's
    piece is checked if the player's king is in its attack range.

 Information about castling is not stored since it is
 not defined for the evochess.
-}
{-# LANGUAGE FlexibleInstances #-}

module Lib where

import Data.List
import Data.Array
import Data.Maybe ( fromJust )

-- Min number of pawn moves to promote #1
_N = 3
-- Min number of piece captures to promote #2
_K = 2

data Kind = Rook | Knight | Bishop | Queen | King | Pawn
  deriving Eq

data Color = White | Black
  deriving Eq

type Square = Maybe Piece

data Piece = Piece Color Kind
  deriving Eq

type Coord = (Int, Int)

data Game = Game { player1 :: Player
                 , player2 :: Player
                 , turn :: Int
                 , board :: Board }

initial = Game { player1 = player0 { color = White }
               , player2 = player0 { color = Black }
               , turn = 1
               , board = evoboard8 }

data Player = Player { pawnMoves :: Int
                     , color :: Color
                     , capturedPieces :: Int}

player0 = Player { pawnMoves = 0
                 , color = White
                 , capturedPieces = 0 }

type Move = (Coord, Coord, Attr Char)
data Attr a = None | Promote a

play :: Move -> Game -> Either String Game
play m g@Game
  { turn = turn
  , player1 = player1
  , player2 = player2
  , board = board } | validMove m activePlayer board = Right g'
                    | otherwise = Left "Invalid move"
  where g' = g { turn = turn + 1
               , player1 = player1'
               , player2 = player2'
               , board = board' }

        -- Update players state
        player1' = if odd turn then activePlayer' else player1
        player2' = if even turn then player2 else activePlayer'
        activePlayer = if odd turn then player1 else player2 :: Player
        activePlayer' = countMoves activePlayer move

        -- Update the board state
        board' = board        -- TODO

countMoves player move = player  -- TODO

validMove (c1, c2, at) player b =
  -- TODO Check the attributes at
  inBoard c1 && inBoard c2 && canMoveTo c1 c2 board && myPiece c1 player board

canMoveTo c1 c2 board = True  -- TODO

myPiece c1 player board = True  -- TODO: is this check necessary?

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

minX, maxX, minY, maxY :: Int
minX = 1
maxX = 8
minY = 1
maxY = 8
boardBounds :: ((Int, Int), (Int, Int))
boardBounds = ((minX, minY), (maxX, maxY))

instance Show Board where
  show (Board b) = concat $ intersperse "\n" s
    where s = [(intersperse ' ' [(head $ showS $ b ! (maxX - i + 1, j)) | j <- [minY..maxY]]) | i <- [minX..maxX]]

newtype Board = Board { getBoard :: Array Coord Square }

inBoard :: Coord -> Bool
inBoard (x, y) = x >= minX && x <= maxX
                 && y >= minY && y <= maxY
{-# INLINE inBoard #-}

-- Pieces in the very beginning of the game on a classical 8x8 board
board8 :: Board
board8 = Board $ listArray ((1, 1), (8, 8)) b
  where
    b = bw ++ pw ++ b0 ++ b0 ++ b0 ++ b0 ++ pb ++ bb
    bw = map Just [Piece White Rook, Piece White Knight, Piece White Bishop, Piece White Queen
      , Piece White King, Piece White Bishop, Piece White Knight, Piece White Rook]
    bb = map Just [Piece Black Rook, Piece Black Knight, Piece Black Bishop, Piece Black Queen
      , Piece Black King, Piece Black Bishop, Piece Black Knight, Piece Black Rook]
    pw = replicate 8 (Just $ Piece White Pawn)
    pb = replicate 8 (Just $ Piece Black Pawn)
    b0 = replicate 8 Nothing

evoboard8 :: Board
evoboard8 = Board $ listArray ((1, 1), (8, 8)) b
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

-- Do not forget en passant
move :: Board -> Coord -> Coord -> Board
move _ _ _ = emptyBoard

-- Move geometry based on the partial game state (no en passant, no castling)
geometry :: Board -> Piece -> Coord -> [Coord]
geometry _ (Piece _ Knight) (x, y) = [ (x - 1, y + 2)
                                     , (x + 1, y + 2)
                                     , (x + 2, y - 1)
                                     , (x + 2, y + 1)
                                     , (x - 1, y - 2)
                                     , (x + 1, y + 2)
                                     , (x - 2, y - 1)
                                     , (x - 2, y + 1) ]

-- Now, the idea is to use the board state and boardBounds
-- in order to get piece's visibility range.
geometry board (Piece _ Bishop) (x, y) = []

geometry board (Piece _ Rook) (x, y) = []

geometry board (Piece color Queen) coord = bishop ++ rook
  where bishop = geometry board (Piece color Bishop) coord
        rook = geometry board (Piece color Rook) coord

geometry _ (Piece _ King) (x, y) = [ (x + 1, y    )
                                   , (x - 1, y    )
                                   , (x    , y + 1)
                                   , (x    , y - 1)
                                   , (x + 1, y + 1)
                                   , (x + 1, y - 1)
                                   , (x - 1, y + 1)
                                   , (x - 1, y - 1) ]

geometry board (Piece White Pawn) (x, y) = [(x, y + 1)] ++ attack1 ++ attack2 ++ longmove
  where
    attack coord' | (board `hasEnemyPiece` White) coord' = [coord']
                  | otherwise = []

    attack1 = attack (x + 1, y + 1)
    attack2 = attack (x - 1, y + 1)
    longmove | y == 2 = [(x, 4)]
             | otherwise = []

geometry board (Piece Black Pawn) (x, y) = [(x, y - 1)] ++ attack1 ++ attack2 ++ longmove
  where
    attack coord' | (board `hasEnemyPiece` Black) coord' = [coord']
                  | otherwise = []

    attack1 = attack (x + 1, y - 1)
    attack2 = attack (x - 1, y - 1)
    longmove | y == 7 = [(x, 5)]
             | otherwise = []

-- Check if the square contains a piece of given color
hasMyPiece :: Board -> Color -> Coord -> Bool
hasMyPiece board mycolor coord = f sq
  where sq = getSquare board coord
        f (Just (Piece mycolor _)) = True
        f _ = False

hasEnemyPiece :: Board -> Color -> Coord -> Bool
hasEnemyPiece board mycolor coord = hasMyPiece board (opposite mycolor) coord
