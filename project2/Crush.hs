-- CPSC 312 - Project 2
-- by Khurram Ali Jaffery

-- ==================================================================
-- CPSC 312 Project 2
-- Dec 4th, 2015

-- Name: Sam Sung
-- student number: 13714126
-- ugrad id: u2u8

-- Name: Rob Wu
-- student number: 42764118
-- ugrad id: y4d8
-- ==================================================================

-- Main Components:
-- minimax algorithm
-- a board evaluator
-- state search
-- movement generators (and by extension, tree generator, new state generator)
-- crusher
-- custom data types (already done)

-- Piece is a data representation of possible pieces on a board
-- where D is an empty spot on the board
--		 W is a piece of the White player
--		 B is a piece of the Black player
--

data Piece = D | W | B deriving (Eq, Show)

--
-- Point is a tuple of 2 elements
-- representing a point on a grid system
-- where the first element represents the x coordinate
--       the second element represents the y coordinate
--

type Point = (Int, Int)

--
-- Tile is a tuple of 2 elements 
-- representing what a point is occupied by
-- where the first element represents a piece 
--       the second element represents a point
--

type Tile  = (Piece, Point)

--
-- Board is a list of Pieces, thus it is an internal representation
-- of the provided string representation of the board, it maintains
-- the same order as the string representation of the board
--

type Board = [Piece]

--
-- Grid is a list of Points, thus it is an internal representation
-- of the hexagonal grid system translated into a coordinate 
-- system to easily maintain and make moves on the board
--

type Grid = [Point]

--
-- State is a list of Tile, thus it is an internal representation precisely
-- for the purposes of zipping the board and the grid together in order
-- to keep easier track of the effects on the pieces of making moves on grid
--

type State = [Tile]

--
-- Next is a data representation for storing and passing around information within
-- the tree generating function, allowing it to correctly generate new children
-- 
-- Next consists of 4 elements
-- where usedDepth is an integer reprsenting the current depth level
--		 newBoard is the next board to add to the tree
-- 		 seenBoards is the updated history to avoid possible future trouble boards
-- 		 cplayer is the current player for whom the board was generated for
--

data Next a = Next {usedDepth :: Int, newBoard :: a, seenBoards :: [a], cplayer :: Piece}

--
-- Tree is a data representation for the search tree, it is an extention of 
-- the rose tree widely used for implementing such unequally branched search trees
--
-- Tree consists of 3 elements
-- where depth is an integer representing the depth level of the node
-- 		 board is the game state at that node
-- 		 nextBoards are the child nodes of the current node
--

data Tree a = Node {depth :: Int, board :: a, nextBoards :: [Tree a]} deriving (Show)

--
-- BoardTree is the internal representation of the search tree of the given board
-- that is to be generatated for correctly implementing the minimax algorithm.
--

type BoardTree = Tree Board

--
-- Slide is a tuple of 2 elements
-- an internal representation of a slide
-- where the first element represents the point to move from
-- 		 the second element represents the adjacent point to move to
--

type Slide = (Point,Point)

--
-- Jump is a tuple of 2 elements
-- an internal representation of a leap
-- where the first element represents the point to move from
-- 		 the second element represents the adjacent point to move over
--		 the third element represents the point to move to
--

type Jump = (Point,Point,Point)

--
-- Move is a tuple of 2 elements
-- an internal representation of a move
-- where the first element represents the point to move from
-- 		 the second element represents the point to move to
--
-- Note: in essence it is the same as a slide however the idea
--		 is that a jump can be reduced to a move as in effect 
--		 nothing happens the point moved over in a jump
--

type Move = (Point,Point)

--
-- Some test results to see what functions are producing 
--
run = crusher ["W------------BB-BBB","----W--------BB-BBB","-W-----------BB-BBB"] 'W' 2 3
run0 = crusher ["WWW-WW-------BB-BBB"] 'W' 2 3
grid0 = generateGrid 3 2 4 []
slides0 = generateSlides grid0 3
jumps0 = generateLeaps grid0 3
board0 = sTrToBoard "WWW-WW-------BB-BBB"
newBoards0 = generateNewStates board0 [] grid0 slides0 jumps0 W
tree0 = generateTree board0 [] grid0 slides0 jumps0 W 4 3
heuristic0 = boardEvaluator W [] 3

newBoards1 = generateNewStates board1 [] grid0 slides0 jumps0 W
board1 = [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]

--
-- crusher
--
-- This function consumes a list of boards, a player, the depth of 
-- search tree, the size of the provide boards, and produces the 
-- next best board possible for the provided player, and accordingly
-- makes the move and returns new board consed onto the list of boards
--
-- Arguments:
-- -- (current:old): current represents the most recent board, old is
--                   the history of all boards already seen in game
-- -- p: 'W' or 'B' representing the player the program is
-- -- d: an Integer indicating depth of search tree
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: a list of String with the new current board consed onto the front
--
{-
*Main> crusher ["W---W----WBB-B-----","W---W----WB--B-B---","---WW----WB--B-B---","---WWB---WB--B-----","W---WB---WB--B-----","W---WB---W---B-B---","-W--WB---W---B-B---","-W--WB---W---B----B","W---WB---W---B----B","W---WB---W-------BB","-W--WB---W-------BB","-W--WW---W---B---BB","-W-WWB---W---B---BB","-W-WWW---W---B--BBB","-WWWWW---B---B--BBB","-WWWWW-------BB-BBB","WWW-WW-------BB-BBB"] 'W' 2 3
["W---WW----BB-B-----","W---W----WBB-B-----","W---W----WB--B-B---","---WW----WB--B-B---","---WWB---WB--B-----","W---WB---WB--B-----","W---WB---W---B-B---","-W--WB---W---B-B---","-W--WB---W---B----B","W---WB---W---B----B","W---WB---W-------BB","-W--WB---W-------BB","-W--WW---W---B---BB","-W-WWB---W---B---BB","-W-WWW---W---B--BBB","-WWWWW---B---B--BBB","-WWWWW-------BB-BBB","WWW-WW-------BB-BBB"]
*Main> crusher ["W---WW----B--B-B---","W---WW----BB-B-----","W---W----WBB-B-----","W---W----WB--B-B---","---WW----WB--B-B---","---WWB---WB--B-----","W---WB---WB--B-----","W---WB---W---B-B---","-W--WB---W---B-B---","-W--WB---W---B----B","W---WB---W---B----B","W---WB---W-------BB","-W--WB---W-------BB","-W--WW---W---B---BB","-W-WWB---W---B---BB","-W-WWW---W---B--BBB","-WWWWW---B---B--BBB","-WWWWW-------BB-BBB","WWW-WW-------BB-BBB"] 'W' 2 3
["W-W-W-----B--B-B---","W---WW----B--B-B---","W---WW----BB-B-----","W---W----WBB-B-----","W---W----WB--B-B---","---WW----WB--B-B---","---WWB---WB--B-----","W---WB---WB--B-----","W---WB---W---B-B---","-W--WB---W---B-B---","-W--WB---W---B----B","W---WB---W---B----B","W---WB---W-------BB","-W--WB---W-------BB","-W--WW---W---B---BB","-W-WWB---W---B---BB","-W-WWW---W---B--BBB","-WWWWW---B---B--BBB","-WWWWW-------BB-BBB","WWW-WW-------BB-BBB"]
-}

crusher :: [String] -> Char -> Int -> Int -> [String]
crusher (current:old) p d n = 
  (boardToStr (stateSearch board history grid 
  (generateSlides grid n) -- generateSlides
  (generateLeaps grid n) -- generateLeaps
  player d n)):(current:old) -- add new board to the head of the previous boards
  
    where
      board = sTrToBoard current -- convert string to board
      history = [sTrToBoard x | x <- old] -- convert [string] to [board]
      grid = generateGrid n (n - 1) (2 * (n - 1)) [] -- generate a default grid
      player = charToPlayer p -- convert player character to player
      
charToPlayer :: Char -> Piece
charToPlayer char
  | char == 'W' = W
  | char == 'B' = B
  | otherwise = error "unexpected player char"


--
-- gameOver
--
-- This function consumes a board, a list of boards, and the dimension
-- of board and determines whether the given board is in a state where
-- the game has ended by checking if the board is present in the provided
-- list of boards or either the W or B pieces are less than dimension of board
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: True if the board is in a state where the game has ended, otherwise False
--

gameOver :: Board -> [Board] -> Int -> Bool
gameOver board history n  -- To Be Completed
    | elem board history             = True   -- if board is part of history 
    | (numOfPieces board W) < n      = True   -- if num of W is less than dim n
    | (numOfPieces board B) < n      = True   -- if num of B is less than dim n
    | otherwise                      = False  -- all good


-- A helper function for gameOver to count the number of pieces
numOfPieces :: Board -> Piece -> Int
numOfPieces [] piece = 0                      -- empty board with no piece ret 0
numOfPieces (h:t) piece
    | h == piece       = 1 + numOfPieces t piece   -- if piece eq specified +1
    | otherwise        = numOfPieces t piece       -- move on


-- Test for gameOver
-- gameOver [W,W,W,B,B,B,W] [] 3 
-- False
--
-- gameOver [W,W,W,B,B,B,W] [] 4
-- True

--
-- sTrToBoard
--
-- This function consumes a list of characters which can be either 'W' or 'B'
-- or '-' and converts them to a list of pieces, i.e W or B or D respectively
--
-- Arguments:
-- -- s: the String to convert into piece-wise representation
--
-- Note: This function would convert "WWW-WW-------BB-BBB" to
-- 	     [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]
--
-- Returns: the Board corresponding to the string
--

sTrToBoard :: String  -> Board
sTrToBoard s = map (\ x -> check x) s
	where 
		check 'W' = W
		check 'B' = B
		check '-' = D

--
-- boardToStr
--
-- This function consumes a board which is a list of either W or B  or D and 
-- converts them to a list of characters, i.e 'W' or 'B' or 'D' respectively
--
-- Arguments:
-- -- b: the Board to convert into char-wise representation
--
-- Note: This function would convert [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B] 
-- 	     to "WWW-WW-------BB-BBB"
--
-- Returns: the String corresponding to the board 
--

boardToStr :: Board -> String
boardToStr b = map (\ x -> check x) b
	where 
		check W = 'W'
		check B = 'B'
		check D = '-'

--
-- generateGrid
--
-- This function consumes three integers (described below) specifying how to
-- properly generate the grid and also a list as an accumulator; to generate a
-- regular hexagon of side length n, pass n (n- 1) (2 * (n - 1)) and []
--
-- Arguments:
-- -- n1: one more than max x-coordinate in the row, initialized always to n
-- -- n2: the number of rows away from the middle row of the grid
-- -- n3: the current y-coordinate i.e the current row number
-- -- acc: an accumulator that keeps track of accumulating rows of grid 
--		   initialized to []
--
-- Note: This function on being passed 3 2 4 [] would produce
--		 [(0,0),(1,0),(2,0)
--		  (0,1),(1,1),(2,1),(3,1)
--		  (0,2),(1,2),(2,2),(3,2),(4,2)
--		  (0,3),(1,3),(2,3),(3,3)
--		  (0,4),(1,4),(2,4)]
--
-- Returns: the corresponding Grid i.e the acc when n3 == -1
--

generateGrid :: Int -> Int -> Int -> Grid -> Grid
generateGrid n1 n2 n3 acc 
	| n3 == -1             = acc
	| otherwise 	         = generateGrid nn1 (n2 - 1) (n3 - 1) (row ++ acc)
		where
			row = map (\ x -> (x,n3)) [0 .. (n1 - 1)]
			nn1 = if n2 > 0 then n1 + 1 else n1 - 1

--
-- generateSlides
--
-- This function consumes a grid and the size of the grid, accordingly
-- generates a list of all possible slides from any point on the grid to
-- any adjacent point on the grid
--
-- Arguments:
-- -- b: the Grid to generate slides for 
-- -- n: an Integer representing the dimensions of the grid
-- 
-- Note: This function is only called at the initial setup of the game, 
-- 		 it is a part of the internal representation of the game, this 
--		 list of all possible slides is only generated once; and when 
-- 		 generating next moves, the program decides which slides out of 
--		 all these possible slides could a player actually make
--
-- Returns: the list of all Slides possible on the given grid
-- 
-- Grid = [Point] 
-- Slide = (Point, Point) 
-- Point = (Int, Int)

-- for all point in top half, the direct neighbor point is 
--        | [0 -1]  [-1 0]  [+1 0]  [0 +1]  [+1 -1]  [-1 +1]   Valid

-- for all point in bottom half, the direct neighbor point is 
--        | [0 -1]  [-1 0]  [+1 0]  [0 +1]  [+1 -1]  [-1 +1]   Valid
-- ------------------------------------------------------------------
--  (2,3) | (2,2)    (1,3)   (3,3)   (2,4)   (3,2)    (1,4)      V
--  (1,3) | (1,2)    (0,3)   (2,3)   (1,4)   (2,2)    (0,4)      V 
--  (1,4) | (1,3)    (0,4)   (2,4)   (1,5)   (2,3)    (0,5)      V 

-- for all row 2, we need a different fuction
--        | [0 -1]  [-1 0]  [+1 0]  [0 +1]  [-1 -1]  [-1 +1]   Valid
-- ------------------------------------------------------------------
--  (2,2) | (2,1)    (1,2)   (3,2)   (2,3)   (1,1)    (1,3)      V
--  (1,2) | (1,1)    (0,2)   (2,2)   (1,3)   (0,1)    (0,3)      V
--
--          ___ ___ ___
--   
--             N   N
--        ___ ___ ___ ___
--   
--           N   X  N
--      ___ ___ ___ ___ ___
--   
--             N   N
--        ___ ___ ___ ___
--   
--
--          ___ ___ ___

generateSlides :: Grid -> Int -> [Slide]
generateSlides b n
    | null b           = []
    | n == 0           = []
    | otherwise        = generateSlideHelper b b n []


-- helper function with accumulator for all the slides for all points
generateSlideHelper :: Grid -> Grid -> Int -> [Slide] -> [Slide]
generateSlideHelper g1 g2 n acc
    | null g1                                    = acc         -- return all done
    | otherwise                                  = generateSlideHelper (tail g1) g2 n newacc
    where newacc = listMerge acc (valideSlideGenerator (head g1) n g2)
 
 -- caller
valideSlideGenerator :: Point -> Int -> Grid -> [Slide]
valideSlideGenerator p n g = gshPointHelper p (gshAllNeighbor p n) g []

-- append function to merge 2 lists together
listMerge :: [a] -> [a] -> [a]
listMerge newlist oldlist 
   | null newlist        = oldlist        -- no need to append
   | null oldlist        = newlist        -- no need to append
   | otherwise           = listMerge (init newlist) (last newlist:oldlist)  -- append in fashion

-- a point specific function
-- helper function to filter out if the possible neighbors are actually within in defined board 
-- if so, create a Slide and add it to the accumulator
gshPointHelper :: Point -> [Point] -> Grid -> [Slide] -> [Slide]
gshPointHelper p lop grid acc
   | null lop                                   = acc        -- all done
   | null grid                                  = []         -- not necessary
   | elem (last lop) grid                       = gshPointHelper p (init lop) grid ((p, last lop):acc)  -- tail rec, recurse on list of point + good slide
   | otherwise                                  = gshPointHelper p (init lop) grid acc -- tail rec, recurse on list of point + bad slide

-- helper function to generate all possible neighbor (all 6 of them)
-- specific case for row 2 as stated above
gshAllNeighbor :: Point -> Int -> [Point] 
gshAllNeighbor (a,b) n
   | b < n-1                                    = [(a,b-1),(a-1,b),(a+1,b),(a,b+1),(a+1,b+1),(a-1,b-1)]  -- specific for top half
   | b == n-1                                   = [(a,b-1),(a-1,b),(a+1,b),(a,b+1),(a-1,b-1),(a-1,b+1)]  -- specific for row dim - 1
   | otherwise                                  = [(a,b-1),(a-1,b),(a+1,b),(a,b+1),(a+1,b-1),(a-1,b+1)]  -- all other rows

{-
Sample Return
generateSlides [(0,0),(1,0),(2,0),(0,1),(1,1),(2,1),(3,1),(0,2),(1,2),(2,2),(3,2),(4,2),(0,3),(1,3),(2,3),(3,3),(0,4),(1,4),(2,4)] 3

[((0,0),(1,0)),((0,0),(0,1)),((0,0),(1,1)),((1,0),(0,0)),((1,0),(2,0)),((1,0),(1,1)),((1,0),(2,1)),((2,0),(1,0)),((2,0),(2,1)),((2,0),(3,1)),((0,1),(0,0)),((0,1),(1,1)),((0,1),(0,2)),((0,1),(1,2)),((1,1),(1,0)),((1,1),(0,1)),((1,1),(2,1)),((1,1),(1,2)),((1,1),(2,2)),((1,1),(0,0)),((2,1),(2,0)),((2,1),(1,1)),((2,1),(3,1)),((2,1),(2,2)),((2,1),(3,2)),((2,1),(1,0)),((3,1),(2,1)),((3,1),(3,2)),((3,1),(4,2)),((3,1),(2,0)),((0,2),(0,1)),((0,2),(1,2)),((0,2),(0,3)),((1,2),(1,1)),((1,2),(0,2)),((1,2),(2,2)),((1,2),(1,3)),((1,2),(0,1)),((1,2),(0,3)),((2,2),(2,1)),((2,2),(1,2)),((2,2),(3,2)),((2,2),(2,3)),((2,2),(1,1)),((2,2),(1,3)),((3,2),(3,1)),((3,2),(2,2)),((3,2),(4,2)),((3,2),(3,3)),((3,2),(2,1)),((3,2),(2,3)),((4,2),(3,2)),((4,2),(3,1)),((4,2),(3,3)),((0,3),(0,2)),((0,3),(1,3)),((0,3),(0,4)),((0,3),(1,2)),((1,3),(1,2)),((1,3),(0,3)),((1,3),(2,3)),((1,3),(1,4)),((1,3),(2,2)),((1,3),(0,4)),((2,3),(2,2)),((2,3),(1,3)),((2,3),(3,3)),((2,3),(2,4)),((2,3),(3,2)),((2,3),(1,4)),((3,3),(3,2)),((3,3),(2,3)),((3,3),(4,2)),((3,3),(2,4)),((0,4),(0,3)),((0,4),(1,4)),((0,4),(1,3)),((1,4),(1,3)),((1,4),(0,4)),((1,4),(2,4)),((1,4),(2,3)),((2,4),(2,3)),((2,4),(1,4)),((2,4),(3,3))]


-}


--
-- generateLeaps
--
-- This function consumes a grid and the size of the grid, accordingly
-- generates a list of all possible leaps from any point on the grid over
-- any adjacent point on the grid to any point next to the adjacent point
-- such that it is movement in the same direction
--
-- Arguments:
-- -- b: the Grid to generate leaps for 
-- -- n: an Integer representing the dimensions of the grid
-- 
-- Note: This function is only called at the initial setup of the game, 
-- 		 it is a part of the internal representation of the game, this 
--		 list of all possible leaps is only generated once; and when 
-- 		 generating next moves, the program decides which leaps out of 
--		 all these possible leaps could a player actually make
--
-- Returns: the list of all Jumps possible on the given grid
--

generateLeaps :: Grid -> Int -> [Jump]
generateLeaps b n
    | null b               = []            -- null grid = no jumps
    | n == 0               = []            -- no dim ?
    | otherwise            = generateLeapsHelper b b n []

-- core helper function that loops through all points inside the grid
generateLeapsHelper :: Grid -> Grid -> Int -> [Jump] -> [Jump]
generateLeapsHelper g1 g2 n acc
    | null g1             = acc           -- all points on the grid processed
    | otherwise           = generateLeapsHelper (tail g1) g2 n newacc
    where newacc = validLeapGenerator (head g1) g2 n acc 

-- helper function that input 2 possible lists (slides and jumps) and perform append on valide data
validLeapGenerator :: Point -> Grid -> Int -> [Jump] -> [Jump] 
validLeapGenerator p g n record = listMerge record jumps                      -- merge jumps
      where jumps = vlgHelper p (gshAllNeighbor p n) (glhAllLeap p n) g n []

-- helper function that perform validity check on the given data
-- the given data is the generated (all possible slides) as well as all possible jumps from a single point
vlgHelper :: Point -> [Point] -> [Point] -> Grid -> Int -> [Jump] -> [Jump]
vlgHelper p slide leap g n acc
    | null slide || null leap                                 = acc        -- if either list goes empty, end it
    | (elem (last slide) g) && (elem (last leap) g)           = vlgHelper p (init slide) (init leap) g n ((p,(last slide),(last leap)):acc)  -- valid leap add it
    | otherwise                                               = vlgHelper p (init slide) (init leap) g n acc  -- in valid leap, ignore
    where 

-- the main generator that generate all possible leaps from a single point
-- can use it with any n valude
glhAllLeap :: Point -> Int -> [Point]
glhAllLeap (a,b) n
    | b < n-2           = [(a,b-2),(a-2,b),(a+2,b),(a,b+2),(a+2,b+2),(a-2,b-2)]    -- all above rows
    | b == n-2          = [(a,b-2),(a-2,b),(a+2,b),(a-1,b+2),(a+1,b+2),(a-2,b-2)]  -- specific for the row directly above center
    | b == n-1          = [(a,b-2),(a-2,b),(a+2,b),(a,b+2),(a-2,b-2),(a-2,b+2)]    -- specific for row dim - 1
    | b == n            = [(a-1,b-2),(a-2,b),(a+2,b),(a,b+2),(a+1,b-2),(a-2,b+2)]    -- specific for the row directly below center
    | otherwise         = [(a,b-2),(a-2,b),(a+2,b),(a,b+2),(a+2,b-2),(a-2,b+2)]    -- all below rows


{-
Sample output
generateLeaps [(0,0),(1,0),(2,0),(0,1),(1,1),(2,1),(3,1),(0,2),(1,2),(2,2),(3,2),(4,2),(0,3),(1,3),(2,3),(3,3),(0,4),(1,4),(2,4)] 3

[((0,0),(1,0),(2,0)),((0,0),(0,1),(0,2)),((0,0),(1,1),(2,2)),((1,0),(1,1),(1,2)),((1,0),(2,1),(3,2)),((2,0),(1,0),(0,0)),((2,0),(2,1),(2,2)),((2,0),(3,1),(4,2)),((0,1),(1,1),(2,1)),((0,1),(1,2),(1,3)),((1,1),(2,1),(3,1)),((1,1),(1,2),(0,3)),((1,1),(2,2),(2,3)),((2,1),(1,1),(0,1)),((2,1),(2,2),(1,3)),((2,1),(3,2),(3,3)),((3,1),(2,1),(1,1)),((3,1),(3,2),(2,3)),((0,2),(0,1),(0,0)),((0,2),(1,2),(2,2)),((0,2),(0,3),(0,4)),((1,2),(1,1),(1,0)),((1,2),(2,2),(3,2)),((1,2),(1,3),(1,4)),((2,2),(2,1),(2,0)),((2,2),(1,2),(0,2)),((2,2),(3,2),(4,2)),((2,2),(2,3),(2,4)),((2,2),(1,1),(0,0)),((2,2),(1,3),(0,4)),((3,2),(2,2),(1,2)),((3,2),(2,1),(1,0)),((3,2),(2,3),(1,4)),((4,2),(3,2),(2,2)),((4,2),(3,1),(2,0)),((4,2),(3,3),(2,4)),((0,3),(1,3),(2,3)),((0,3),(1,2),(1,1)),((1,3),(1,2),(0,1)),((1,3),(2,3),(3,3)),((1,3),(2,2),(2,1)),((2,3),(2,2),(1,1)),((2,3),(1,3),(0,3)),((2,3),(3,2),(3,1)),((3,3),(3,2),(2,1)),((3,3),(2,3),(1,3)),((0,4),(0,3),(0,2)),((0,4),(1,4),(2,4)),((0,4),(1,3),(2,2)),((1,4),(1,3),(1,2)),((1,4),(2,3),(3,2)),((2,4),(2,3),(2,2)),((2,4),(1,4),(0,4)),((2,4),(3,3),(4,2))]
 -}

--
-- stateSearch
--
-- This function consumes the arguments described below, based on the internal
-- representation of the game, if there is no point in playing the game as the
-- current board is in a state where the game has ended then just return the 
-- board, else generate a search tree till the specified depth and apply 
-- minimax to it by using the appropriately generated heuristic
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
-- -- depth: an Integer indicating depth of search tree
-- -- num: an Integer representing the dimensions of the board
--
-- Returns: the current board if game is over, 
--          otherwise produces the next best board
--

stateSearch :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> Board
stateSearch board history grid slides jumps player depth num
    | gameOver board history num                    = board     -- game over just return the board
    | otherwise                                     = minimax genTree heuristic  -- game not over find tree and their hu
    where genTree = generateTree board history grid slides jumps player depth num -- generate a search tree
          heuristic = boardEvaluator player history num -- hueristic to be used
 

sstest0 = stateSearch [D,W,D,W,W,W,D,W,D,D,D,D,D,B,B,D,B,B,B] [[D,D,W,W,W,W,D,W,D,D,D,D,D,B,B,D,B,B,B],[W,D,W,W,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B],[D,W,W,W,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B],[W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]] grid0 slides0 jumps0 W 2 3
{-
[W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]
[D,W,W,W,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]
[W,D,W,W,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]
[D,D,W,W,W,W,D,W,D,D,D,D,D,B,B,D,B,B,B]
[D,W,D,W,W,W,D,W,D,D,D,D,D,B,B,D,B,B,B]
[W,D,D,W,W,W,D,W,D,D,D,D,D,B,B,D,B,B,B]
-}

--
-- generateTree
--
-- This function consumes the arguments described below, and builds a search
-- tree till specified depth from scratch by using the current board and
-- generating all the next states recursively; however it doesn't generate
-- children of those states which are in a state where the game has ended.
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
-- -- depth: an Integer indicating depth of search tree
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: the corresponding BoardTree generated till specified depth
--
mm'test1 = minimax' gtTest0 (boardEvaluator W [[W,D,D,W,W,D,D,D,W,W,D,D,D,B,D,B,D,B,B],[D,D,W,W,W,W,D,W,D,D,D,D,D,B,B,D,B,B,B],[W,D,W,W,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B],[D,W,W,W,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B],[W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]] 3) True
mm'test0 = minimax' gtTest0 (boardEvaluator W [[W,D,D,W,W,D,D,D,W,W,D,D,D,B,D,B,D,B,B],[D,D,W,W,W,W,D,W,D,D,D,D,D,B,B,D,B,B,B],[W,D,W,W,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B],[D,W,W,W,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B],[W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]] 3) False
mmTest0 = minimax gtTest0 (boardEvaluator W [[W,D,D,W,W,D,D,D,W,W,D,D,D,B,D,B,D,B,B],[D,D,W,W,W,W,D,W,D,D,D,D,D,B,B,D,B,B,B],[W,D,W,W,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B],[D,W,W,W,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B],[W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]] 3)
gtTest0 = generateTree [W,D,D,D,W,D,D,D,W,W,D,D,D,W,D,B,D,B,B] [[W,D,D,W,W,D,D,D,W,W,D,D,D,B,D,B,D,B,B],[D,D,W,W,W,W,D,W,D,D,D,D,D,B,B,D,B,B,B],[W,D,W,W,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B],[D,W,W,W,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B],[W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]] grid0 slides0 jumps0 W 3 2

generateTree :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> BoardTree
generateTree board history grid slides jumps player depth n
    | depth == 0                          = Node {depth = depth, board = board, nextBoards = []}       -- depth reached, end it
    | otherwise                           = Node {depth = depth, board = board, nextBoards = children} -- generate tree children
    where newStates = generateNewStates board history grid slides jumps player -- get new states from current board
          children = processNewStates newStates board history grid slides jumps player (depth-1) n  -- gen children and sub depth by 1

-- this fucntion processes the new states generated from the parent node
-- up on all children node, it will generate its children node until depth is reached 
processNewStates :: [Board] -> Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> [BoardTree]
processNewStates ns b h g s j p d n = [generateTree newBoard h g s j nextPlayer d n | newBoard <- ns]  -- generate each nb's children nodes if depth
    where nextPlayer = togglePlayer p  -- switch sides

-- Test data
-- custom made grid
-- 
--           W   D   W
--          ___ ___ ___
--  
--         D   W   B   B
--        ___ ___ ___ ___
--   
--           B   D   D
--          ___ ___ ___

gtTestGrid = [(0,0),(1,0),(2,0),(0,1),(1,1),(2,1),(3,1),(0,2),(1,2),(2,2)]
gtTestSlide = generateSlides  [(0,0),(1,0),(2,0),(0,1),(1,1),(2,1),(3,1),(0,2),(1,2),(2,2)] 2
gtTestJump = generateLeaps  [(0,0),(1,0),(2,0),(0,1),(1,1),(2,1),(3,1),(0,2),(1,2),(2,2)] 2
gtTestrun = generateTree [W,D,W,D,W,B,B,B,D,D] [] gtTestGrid gtTestSlide gtTestJump W 2 2
--
-- generateNewStates
--
-- This function consumes the arguments described below, it first generates a
-- list of valid moves, applies those moves to the current board to generate 
-- a list of next boards, and then checks whether or not that move would 
-- have been possible by filtering out those boards already seen before
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
--
-- Returns: the list of next boards
--

currentStateTest = mapToState [W,W,W,D] [(0,0),(1,0),(2,0),(0,1)]
gnsTest0 = generateNewStates [D,W,D,W,W,W,D,W,D,D,D,D,D,B,B,D,B,B,B] [[D,D,W,W,W,W,D,W,D,D,D,D,D,B,B,D,B,B,B],[W,D,W,W,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B],[D,W,W,W,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B],[W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]] grid0 slides0 jumps0 W

generateNewStates :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> [Board]
generateNewStates board history grid slides jumps player = newStateGenerator grid board history availableMoves player []
       where currentState = mapToState board grid   -- map the current board into State
             availableMoves = moveGenerator currentState slides jumps player  -- generate moves based on the states given

-- this function generate the current state by mapping pieces and points
mapToState :: Board -> Grid -> State
mapToState board grid
    | null grid || null board    = []      -- end mapping
    | otherwise                  = (head board,head grid):mapToState (tail board) (tail grid)  -- respect the order of the board

-- this function go through each available moves and generate a new board for that move if have not seen before
newStateGenerator :: Grid -> Board -> [Board] -> [Move] -> Piece -> [Board] -> [Board]
newStateGenerator grid board hist moves player acc
    | null grid || null board             = []   -- no need to proceed
    | null moves                          = acc  -- process complete for each move
    | elem newBoard hist                  = newStateGenerator grid board hist (tail moves) player acc  -- already seen dont add
    | otherwise                           = newStateGenerator grid board (newBoard:hist) (tail moves) player (newBoard:acc) -- new board add it 
    where newBoard = applyMove (head moves) board grid player []  -- apply each individual moves 

-- this function apply the move onto a board then returns the board
applyMove :: Move -> Board -> Grid -> Piece -> Board -> Board
applyMove (current,target) board grid player acc
    | null grid                          = acc  -- all done now
    | current == lastgrid                = applyMove (current,target) initboard initgrid player (D:acc) -- remove the current piece
    | target == lastgrid                 = applyMove (current,target) initboard initgrid player (player:acc) -- replace the target piece
    | otherwise                          = applyMove (current,target) initboard initgrid player (lastboard:acc) -- keep the piece
    where initboard = init board  -- all other in front
          lastboard = last board  -- the last item in list
          initgrid = init grid    -- all pos in front
          lastgrid = last grid    -- last pos in list


{-
 Sample output 
 newBoard 
     L0    L1       L2       L3      L4
  |-----|-------|---------|-------|-----|    Valid
 [[D,W,W,W,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B],     V
  [D,W,W,D,W,W,D,D,D,W,D,D,D,B,B,D,B,B,B],     V
  [W,D,W,D,W,W,D,D,D,D,W,D,D,B,B,D,B,B,B],     V
  [W,D,W,D,W,W,D,D,W,D,D,D,D,B,B,D,B,B,B],     V
  [W,W,D,D,W,W,W,D,D,D,D,D,D,B,B,D,B,B,B],     V
  [W,W,D,D,W,W,D,D,D,W,D,D,D,B,B,D,B,B,B],     V
  [W,W,W,D,D,W,D,D,D,W,D,D,D,B,B,D,B,B,B],     V
  [W,W,W,D,D,W,D,D,W,D,D,D,D,B,B,D,B,B,B],     V
  [W,W,W,W,D,W,D,D,D,D,D,D,D,B,B,D,B,B,B],     V
  [W,W,W,D,D,W,W,D,D,D,D,D,D,B,B,D,B,B,B],     V
  [W,W,W,D,W,D,D,D,D,D,W,D,D,B,B,D,B,B,B],     V
  [W,W,W,D,W,D,D,D,D,W,D,D,D,B,B,D,B,B,B],     V
  [W,W,W,D,W,D,W,D,D,D,D,D,D,B,B,D,B,B,B],     V
  [W,W,W,W,W,D,D,D,D,D,D,D,D,B,B,D,B,B,B]]     V

-}


--
-- moveGenerator
--
-- This function consumes a state, a list of possible jumps, 
-- a list of possible slides and a player from whose perspective 
-- to generate moves, to check which of these jumps and slides 
-- the player could actually make, and produces a list of valid moves
--
-- Arguments:
-- -- state: a State representing the most recent state
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
--
-- Note: This is the only instance where the program makes use of the
--		 type State, for our purposes it is zipping the board and the
--		 grid together for making it easier to make moves.
--
-- Note:
-- -- oP is opponentsPieces
-- -- pP is playersPieces
-- -- vS is validSlides
-- -- vJ is validJumps
--
-- Returns: the list of all valid moves that the player could make
--
-- Test data
moveGenTestSlide = generateSlides grid0 3
moveGenTestJump = generateLeaps grid0 3
moveGenTest = moveGenerator  [(W,(0,0)),(W,(1,0)),(W,(2,0)),(D,(0,1)),(W,(1,1)),(D,(2,1)),(D,(3,1)),(D,(0,2)),(W,(1,2)),(D,(2,2)),(D,(3,2)),(D,(4,2)),(D,(0,3)),(B,(1,3)),(B,(2,3)),(B,(3,3)),(D,(0,4)),(B,(1,4)),(B,(2,4))] moveGenTestSlide moveGenTestJump B
{-
 Test Data Sample return (Verified)
 [((2,4),(2,2)),((2,4),(0,4)),((2,4),(4,2)),((1,4),(1,2)),((1,4),(3,2)),((1,4),(0,4)),((3,3),(3,2)),((3,3),(4,2)),((2,3),(0,3)),((2,3),(2,2)),((2,3),(3,2)),((1,3),(0,3)),((1,3),(2,2)),((1,3),(0,4))]
-}


moveGenerator :: State -> [Slide] -> [Jump] -> Piece -> [Move]
moveGenerator state slides jumps player
   | null state              = []                                                                     -- no state, no move
   | otherwise               = moveGenHelper (filterStatePlayer state player) state slides jumps []   -- pass to helper

-- all the possible states with the player color
filterStatePlayer :: State -> Piece -> State
filterStatePlayer state player = filter (\(p,pt) -> p == player) state  -- filter out all non player states

-- all the tiles in (h:t) belong to player
-- further process them such that all the slides and jumps in the list left are from the tiles
moveGenHelper :: State -> State -> [Slide] -> [Jump] -> [Move] -> [Move]
moveGenHelper state s ss js acc
   | null state                                       = acc       -- if no state left, exit
   | otherwise                                        = moveGenHelper (tail state) s ss js newacc  -- recursive on tail
   where h = head state
         newacc = tileProcess h s (filterSlide h ss) (filterJump h js) acc  -- the acc is updated with all legal moves from current tile

-- All the possible slides for this tile
filterSlide :: Tile -> [Slide] -> [Slide]
filterSlide (p,pts) ss = filter (\(a,b) -> a == pts) ss  -- filter out slides that do not originate from the given point

-- All the possible leaps for this tile
filterJump :: Tile -> [Jump] -> [Jump]
filterJump (p,pts) js = filter (\(a,b,c) -> a == pts) js -- filter out jumps that do not originate from the given point

-- all the [slide] and [jump] belong to this specific tile
-- for a given tile, process all possible slides first, then jumps, return when complete
tileProcess :: Tile -> State -> [Slide] -> [Jump] -> [Move] -> [Move]
tileProcess (p,pts) states slides jumps record
   | null slides && null jumps                         = record   -- all done here
   | null slides                                       = tileProcess (p,pts) states slides (tail jumps) newJumpRecord  -- jumps only after slides done
   | otherwise                                         = tileProcess (p,pts) states (tail slides) jumps newSlideRecord -- slides only
   where newJumpRecord =  listMerge (processJumpMove (head jumps) states p) record -- update the list of moves for the given point
         newSlideRecord =  listMerge (processSlideMove (head slides) states) record

-- process and verify that the leap rule is not violated.
-- leap can jump over a B or W but not D, and the target location should be D
processJumpMove :: Jump -> State -> Piece -> [Move]
processJumpMove (p1,p2,p3) s p
   | null piece2                                        = []   -- p2 piece not found
   | piece2 == [D]                                      = []   -- p2 is empty, jump not allowed
   | piece3 == [p] || piece2 /= [p]                     = []   -- p3 is not empty, jump not allowed
   | otherwise                                          = [(p1,p3)]  -- good
   where piece2 = getGamePieceAtPos s p2
         piece3 = getGamePieceAtPos s p3

-- process and verify that the slide rule is not violated.
-- slide can move to a D only.
processSlideMove :: Slide -> State -> [Move]
processSlideMove (p1,p2) s
   | null piece                                        = []     -- p2 not found on board
   | piece == [D]                                      = [(p1,p2)] -- p2 is empty, good
   | otherwise                                         = [] -- p2 not empty, ignore
   where piece = getGamePieceAtPos s p2

-- given a state and a point on the board, find the piece that belongs to that point
getGamePieceAtPos :: State -> Point -> [Piece]
getGamePieceAtPos ((piece,pts):t) p 
   | null ((piece,pts):t)                              = []       -- piece not found
   | pts == p                                          = [piece]  -- piece found, return piece
   | otherwise                                         = getGamePieceAtPos t p -- recursive

{-
  Test:
  I created a simplified board:
 
           W   D   W
          ___ ___ ___
  
         D   W   B   B
        ___ ___ ___ ___
   
           B   D   D
          ___ ___ ___

 The State of this board setup is
 [(W,(0,0)),(D,(1,0)),(W,(2,0)),(D,(0,1)),(W,(1,1)),(B,(2,1)),(B,(3,1)),(B,(0,2)),(D,(1,2)),(D,(2,2))]

 All available Slides [Slides] is
 [((2,2),(3,1)),((2,2),(1,2)),((2,2),(2,1)),
((1,2),(2,1)),((1,2),(2,2)),((1,2),(0,2)),((1,2),(1,1)),
((0,2),(1,1)),((0,2),(1,2)),((0,2),(0,1)),
((3,1),(2,2)),((3,1),(2,0)),((3,1),(2,1)),
((2,1),(1,2)),((2,1),(1,0)),((2,1),(2,2)),((2,1),(3,1)),((2,1),(1,1)),((2,1),(2,0)),
((1,1),(0,2)),((1,1),(0,0)),((1,1),(1,2)),((1,1),(2,1)),((1,1),(0,1)),((1,1),(1,0)),
((0,1),(0,2)),((0,1),(1,1)),((0,1),(0,0)),
((2,0),(3,1)),((2,0),(2,1)),((2,0),(1,0)),
((1,0),(2,1)),((1,0),(1,1)),((1,0),(2,0)),((1,0),(0,0)),
((0,0),(1,1)),((0,0),(0,1)),((0,0),(1,0))] 

All available Jumps [Jumps] is
[((2,2),(1,2),(0,2)),((2,2),(2,1),(1,0)),
((1,2),(2,1),(2,0)),((1,2),(1,1),(0,0)),
((0,2),(1,1),(1,0)),((0,2),(1,2),(2,2)),
((3,1),(2,1),(1,1)),
((2,1),(1,1),(0,1)),
((1,1),(2,1),(3,1)),
((0,1),(1,1),(2,1)),
((2,0),(2,1),(1,2)),((2,0),(1,0),(0,0)),
((1,0),(2,1),(2,2)),((1,0),(1,1),(0,2)),
((0,0),(1,1),(1,2)),((0,0),(1,0),(2,0))] 

Sample Output for Player B:
moveGenerator [(W,(0,0)),(D,(1,0)),(W,(2,0)),(D,(0,1)),(W,(1,1)),(B,(2,1)),(B,(3,1)),(B,(0,2)),(D,(1,2)),(D,(2,2))] [((2,2),(3,1)),((2,2),(1,2)),((2,2),(2,1)),((1,2),(2,1)),((1,2),(2,2)),((1,2),(0,2)),((1,2),(1,1)),((0,2),(1,1)),((0,2),(1,2)),((0,2),(0,1)),((3,1),(2,2)),((3,1),(2,0)),((3,1),(2,1)),((2,1),(1,2)),((2,1),(1,0)),((2,1),(2,2)),((2,1),(3,1)),((2,1),(1,1)),((2,1),(2,0)),((1,1),(0,2)),((1,1),(0,0)),((1,1),(1,2)),((1,1),(2,1)),((1,1),(0,1)),((1,1),(1,0)),((0,1),(0,2)),((0,1),(1,1)),((0,1),(0,0)),((2,0),(3,1)),((2,0),(2,1)),((2,0),(1,0)),((1,0),(2,1)),((1,0),(1,1)),((1,0),(2,0)),((1,0),(0,0)),((0,0),(1,1)),((0,0),(0,1)),((0,0),(1,0))] [((2,2),(1,2),(0,2)),((2,2),(2,1),(1,0)),((1,2),(2,1),(2,0)),((1,2),(1,1),(0,0)),((0,2),(1,1),(1,0)),((0,2),(1,2),(2,2)),((3,1),(2,1),(1,1)),((2,1),(1,1),(0,1)),((1,1),(2,1),(3,1)),((0,1),(1,1),(2,1)),((2,0),(2,1),(1,2)),((2,0),(1,0),(0,0)),((1,0),(2,1),(2,2)),((1,0),(1,1),(0,2)),((0,0),(1,1),(1,2)),((0,0),(1,0),(2,0))] B

      LEAP         SLIDE          SLIDE        SLIDE         LEAP          SLIDE         SLIDE          SLIDE
[((0,2),(1,0)),((0,2),(0,1)),((0,2),(1,2)),((3,1),(2,2)),((2,1),(0,1)),((2,1),(2,2)),((2,1),(1,0)),((2,1),(1,2))]
-}


--
-- boardEvaluator
--
-- This function consumes a board and performs a static board evaluation, by 
-- taking into account whose perspective the program is playing from, the list 
-- of boards already seen, the size of the board, and whether or not it is the
-- program's turn or not; to generate quantitative measures of the board, and 
-- accordingly produce a goodness value of the given board 
--
-- Arguments:
-- -- player: W or B representing the player the program is
-- -- history: a list of Boards of representing all boards already seen
-- -- n: an Integer representing the dimensions of the board
-- -- board: a Board representing the most recent board
-- -- myTurn: a Boolean indicating whether it is the program's turn or the opponents.
--
-- Returns: the goodness value of the provided board
--
-- As seen in class 
-- If player piece # < dim            = lost -100
-- if opponent piece # < dim          = win  +100
-- otherwise the board value is number of your pawns - number of opponent’s pawns

boardEvaluator :: Piece -> [Board] -> Int -> Board -> Bool -> Int
boardEvaluator player history n board myTurn = scoreCalculator board player n

-- this function calculates the score analyze the current board and return a score
scoreCalculator :: Board -> Piece -> Int -> Int
scoreCalculator board player n
   | me < n                                 = -100*n       -- lost
   | op < n                                 =  100*n       -- won
   | otherwise                              =  alive + capture + dead
   where 
         maxPiece = 2*n - 1                  -- piece per player
         me = gameAnalyzer board player      -- my piece score
         oo = togglePlayer player            -- if im white then opponent is B else W
         op = gameAnalyzer board oo          -- opponents piece score
         alive = me*15
         capture = (maxPiece - op)*100
         dead = (maxPiece - me)*(-45)

-- count the number pieces on the board based on the specific player
-- this is the player's number
gameAnalyzer :: Board -> Piece -> Int
gameAnalyzer [] player    = 0
gameAnalyzer _ D          = 0   -- should never happen
gameAnalyzer board player = myLength filteredBoard
      where filteredBoard = filter (\(p) -> p == player) board

-- determine the length of a list. essentially length
myLength :: [a] -> Int
myLength l
  | null l                = 0
  | otherwise             = 1 + myLength (tail l)

-- toggle and alternate between 2 palyers 
togglePlayer :: Piece -> Piece
togglePlayer player 
    | player == W                          = B   -- if white then black
    | player == B                          = W   -- if black then white
    | otherwise                            = W   -- should never happen here

{-
 Sample Output
 boardEvaluator W [] 3 [W,W,B,B,W,B,D] False

 0 

boardEvaluator W [] 3 [W,W,B,D,W,B,D] False

10
-}

--
-- minimax
--
-- This function implements the minimax algorithm, it consumes a search tree, 
-- and an appropriate heuristic to apply to the tree, by applying minimax it
-- produces the next best board that the program should make a move to
--
-- Arguments:
-- -- (Node _ b children): a BoardTree to apply minimax algorithm on
-- -- heuristic: a paritally evaluated boardEvaluator representing the
--				 appropriate heuristic to apply based on the size of the board,
--				 who the program is playing as, and all the boards already seen
--
-- Returns: the next best board
--

minimax :: BoardTree -> (Board -> Bool -> Int) -> Board
minimax (Node _ b children) heuristic
    | null children                 = b  -- no available child nodes from current board, terminate
    | otherwise                     = selectOptimal children heuristicList positionIndex -- find best board
    where heuristicList             = map(\x -> minimax' x heuristic False) children -- start w/ false b/c its computers turn
          positionIndex             = [0..((myLength children) - 1)] -- generating index
 

-- this function select the optimal board from the children node and return to org caller
selectOptimal :: [BoardTree] -> [Int] -> [Int] -> Board
selectOptimal bs hl pl              = board (bs !! i)      -- get the board
    where i                         = selectMax hl pl [0]  -- find index of max

-- this function finds the maximum heuristic value and return its associate index
selectMax :: [Int] -> [Int] -> [Int] -> Int
selectMax hl pl acc
    | null pl                                   = head acc                              -- return index 
    | (hl !! (head pl)) > (hl !! (head acc))    = selectMax hl (tail pl) (head pl:acc)  -- add max
    | otherwise                                 = selectMax hl (tail pl) acc            -- ignore 


--
-- minimax'
--
-- This function is a helper to the actual minimax function, it consumes 
-- a search tree, an appropriate heuristic to apply to the leaf nodes of 
-- the tree, and based on whether it would have been the maximizing 
-- player's turn, it accordingly propogates the values upwards until
-- it reaches the top to the base node, and produces that value.
--
-- Arguments:
-- -- (Node _ b []): a BoardTree
-- -- (Node _ b children): a BoardTree
-- -- heuristic: a paritally evaluated boardEvaluator representing the
--				 appropriate heuristic to apply based on the size of the board,
--				 who the program is playing as, and all the boards already seen
-- -- maxPlayer: a Boolean indicating whether the function should be maximizing
-- 				 or miniziming the goodness values of its children
--
-- Returns: the minimax value at the top of the tree
--

minimax' :: BoardTree -> (Board -> Bool -> Int) -> Bool -> Int
minimax' (Node d board children) heuristic maxPlayer
    | null children                = heuristic board maxPlayer  -- no child nodes, just return the heuristic of current
    | maxPlayer == True            = maximum nextLevel      -- max it
    | otherwise                    = minimum nextLevel      -- min it
    where nextLevel = map(\x -> minimax' x heuristic (not maxPlayer)) children  -- toggle max min 


play :: [String] -> Char -> Int -> Int -> IO ()
play history@(current:old) player depth n
  | gameOver (sTrToBoard current) (map sTrToBoard old) n = putStrLn "Game over."
  | otherwise = do
       let history'@(new:_) = crusher history player depth n
       putStrLn $  "\n" ++ " Board: " ++ "\n"
       prettyPrint (sTrToBoard current)
       play history' (if player == 'W' then 'B' else 'W') depth n


prettyPrint :: Board -> IO()
prettyPrint bd = putStr a
  where
    b = addSpace (boardToStr bd)
    a = "  " ++ take 6 b ++ "\n" ++ 
        " " ++ take 8 (drop 6 b) ++ "\n" ++
        take 10 (drop 14 b) ++ "\n" ++
        " " ++ take 8 (drop 24 b) ++ "\n" ++
        "  " ++ drop 32 b

        
-- addSpace adds one space after each char in a string
addSpace :: String -> String
addSpace xs = if length xs <= 1
              then xs
              else take 1 xs ++ " " ++ addSpace (tail xs)