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

geometry board piece@(Piece _ Queen) coord = bishop ++ rook
  where bishop = geometry board piece coord
        rook = geometry board piece coord

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
