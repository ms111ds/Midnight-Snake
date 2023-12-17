module DrawLibrary where

import Data.Word



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
----------------------------   Helper Functions   -----------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


--------------------------------------------------------------
-- _createRow 
-- Description: Internal function to be called by createGrid2D.
--              Creates a list containing 2D coordinates for a
--              row starting at defined point. Goes in positive.
--              X direction. E.g.
--                 topLeftX = -0.1
--                 topLeftY = 0.9
--                 spacingX = 0.1
--                 numX     = 3
--                 the result is [-0.1, 0.9, 0.0, 0.9, 0.1, 0.9]
-- topLeftX:    The X coordinate of the first point
-- topLeftY:    The Y coordinate of the first point
-- spacingX:    How much the next X,Y value moves in the positve
--              X direction (to the right)
-- numX:        The number of X,Y coordinates to create
-- return:      List of floating point values representing X,Y
--              coordinates. E.g. [-0.1, 0.9,  // 1st X,Y coordinate
--                                  0.0, 0.9,  // 2nd X,Y coordinate 
--                                  0.1, 0.9]  // 3rd X,Y coordinate
--------------------------------------------------------------
_createRow ::Float -> Float -> Float -> Word32 -> [Float]
_createRow topLeftX topLeftY spacingX 0 = []
_createRow topLeftX topLeftY spacingX numX =
   topLeftX:(topLeftY: (_createRow (topLeftX + spacingX)
                                   topLeftY
                                   spacingX
                                  (numX -1) ) )



--------------------------------------------------------------
-- _createLeftTriange
-- Description: inner function to be called by
--               _createIndicesArrayInner2D. Defines a triange
--              for a cell in a 2D grid ( a cell here means
--              the area bounded by 4 adjacent grid points
--              that make a square ). Uses the top left
--              point to create the cell's left triangle. 
-- numX:        number of points in each row of the 2D grid.
-- curIndX:     The column number (where the first column is
--              zero) where the triangle's top left point is.
-- curIndY:     The row number (where the first row is zero)
--              where the triangle's top left point is.
-- return:      A list of 3 indexes representing the left-side
--              triangle. The indexes are into the X,Y
--              coordinates of a grid.
--              E.g. for the grid
--                  [-0.1, 0.9,  // 1st X,Y coordinate / column 0 / row 0 / index 0
--                    0.0, 0.9,  // 2nd X,Y coordinate / column 1 / row 0 / index 1
--                    0.1, 0.9,  // 3rd X,Y coordinate / column 2 / row 0 / index 2
--                   -0.1, 0.7,  // 4th X,Y coordinate / column 0 / row 1 / index 3
--                    0.0, 0.7,  // 5th X,Y coordinate / column 1 / row 1 / index 4
--                    0.1, 0.7]  // 6th X,Y coordinate / column 2 / row 1 / index 5
--                 
--                   * the index of 3 represents the (-0.1, 0.7)
--                   * [0,3,4] represents the left triangle for the first cell
--------------------------------------------------------------
_createLeftTriange numX curIndX curIndY = ( curIndY       * numX + curIndX ) :
                                      ( ( ( curIndY + 1 ) * numX + curIndX ) : 
                                        [ ( curIndY + 1 ) * numX + curIndX + 1 ] )


--------------------------------------------------------------
-- _createRightTriange
-- Description: inner function to be called by
--               _createIndicesArrayInner2D. Defines a triange
--              for a cell in a 2D grid ( a cell here means
--              the area bounded by 4 adjacent grid points
--              that make a square ). Uses the top left
--              point to create the cell's right-side triangle. 
-- numX:        number of points in each row of the 2D grid.
-- curIndX:     The column number (where the first column is
--              zero) where the triangle's top left point is.
-- curIndY:     The row number (where the first row is zero)
--              where the triangle's top left point is.
-- return:      A list of 3 indexes representing the right-side 
--              triangle. The indexes are into the X,Y
--              coordinates of a grid.
--              E.g. for the grid
--                  [-0.1, 0.9,  // 1st X,Y coordinate / column 0 / row 0 / index 0
--                    0.0, 0.9,  // 2nd X,Y coordinate / column 1 / row 0 / index 1
--                    0.1, 0.9,  // 3rd X,Y coordinate / column 2 / row 0 / index 2
--                   -0.1, 0.7,  // 4th X,Y coordinate / column 0 / row 1 / index 3
--                    0.0, 0.7,  // 5th X,Y coordinate / column 1 / row 1 / index 4
--                    0.1, 0.7]  // 6th X,Y coordinate / column 2 / row 1 / index 5
--                 
--                   * the index of 3 represents the (-0.1, 0.7)
--                   * [0,1,4] represents the right triangle for the first cell
--------------------------------------------------------------
_createRightTriange numX curIndX curIndY = ( curIndY       * numX + curIndX ) :
                                         ( ( curIndY       * numX + curIndX + 1 ) : 
                                         [ ( curIndY + 1 ) * numX + curIndX + 1 ] )


--------------------------------------------------------------
-- _createIndicesArrayInner2D
-- Description: inner function to be called by
--              createIndicesArray2D. Defines a trianges
--              for cells in a 2D grid ( a cell here means
--              the area bounded by 4 adjacent grid points
--              that make a square ). Uses the top left
--              point of each cell to create the triangles. 
-- numX:        number of points in each row of the 2D grid.
-- numY:        number of points in each column of the 2D grid.
-- curIndX:     The column number (where the first column is
--              zero) where the triangle's top left point is.
-- curIndY:     The row number (where the first row is zero)
--              where the triangle's top left point is.
-- return:      A list of indexes representing the triangles for 
--              the grid. The indexes are into the X,Y
--              pairs for the grid.
--              E.g. for the grid
--                  [-0.1, 0.9,  // 1st X,Y coordinate / column 0 / row 0 / index 0
--                    0.0, 0.9,  // 2nd X,Y coordinate / column 1 / row 0 / index 1
--                    0.1, 0.9,  // 3rd X,Y coordinate / column 2 / row 0 / index 2
--                   -0.1, 0.7,  // 4th X,Y coordinate / column 0 / row 1 / index 3
--                    0.0, 0.7,  // 5th X,Y coordinate / column 1 / row 1 / index 4
--                    0.1, 0.7]  // 6th X,Y coordinate / column 2 / row 1 / index 5
--                 
--                   * the index of 3 represents the (-0.1, 0.7)
--                   * [0,3,4] represents the left triangle for the first cell
--                   * [0,1,4] represents the right triangle for the first cell
--------------------------------------------------------------
_createIndicesArrayInner2D numX numY curIndX curIndY
   | curIndY == numY - 1 = []
   | curIndX == numX - 1 = _createIndicesArrayInner2D numX numY 0 ( curIndY + 1 )
   | otherwise        = ( _createLeftTriange  numX curIndX curIndY ) ++
                        ( _createRightTriange numX curIndX curIndY ) ++
                        _createIndicesArrayInner2D numX numY ( curIndX + 1 ) curIndY






-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
---------------------------   Library Functions   -----------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


--------------------------------------------------------------
-- createGrid2D 
-- Description: creates a list representing a grid of X,Y
--              coordinates. The grid starts at (topLeftX,topLeftY)
--              and is numX points wide (going right of topLeftX)
--              and numY points high (going down from topLeftY). The
--              distance between each point is determined by
--              spacingX in the X direction and spacingY in the
--              Y direction.
--              E.g.
--                 topLeftX = -0.1
--                 topLeftY = 0.9
--                 spacingX = 0.1
--                 spacingY = 0.2
--                 numX     = 3
--                 numY     = 2
--                 the result is [-0.1, 0.9, 0.0, 0.9, 0.1, 0.9,
--                                -0.1, 0.7, 0.0, 0.7, 0.1, 0.7]
-- topLeftX:    The X coordinate of the first point
-- topLeftY:    The Y coordinate of the first point
-- spacingX:    How much the next X,Y value moves in the positve
--              X direction (to the right)
-- spacingY:    How much the next row of X,Y values moves in the 
--              negative Y direction (down)
-- numX:        The number of X,Y coordinates to create in each row
-- numX:        The number of X,Y coordinates to create in each column
-- return:      List of floating point values representing X,Y
--              coordinates. E.g. [-0.1, 0.9,  // 1st X,Y coordinate
--                                  0.0, 0.9,  // 2nd X,Y coordinate 
--                                  0.1, 0.9,  // 3rd X,Y coordinate
--                                 -0.1, 0.7,  // 4th X,Y coordinate
--                                  0.0, 0.7,  // 5th X,Y coordinate 
--                                  0.1, 0.7]  // 6th X,Y coordinate
--------------------------------------------------------------
createGrid2D :: Float -> Float -> Float -> Float -> Word32 -> Word32 -> [Float] 
createGrid2D _        _        _        _        _    0 = [] 
createGrid2D topLeftX topLeftY spacingX spacingY numX numY =
   ( _createRow topLeftX topLeftY spacingX numX ) ++
   createGrid2D topLeftX (topLeftY - spacingY) spacingX spacingY numX (numY-1)




--------------------------------------------------------------
-- addAttributesToGrid 
-- Description:  Adds attributes ( defined as an array of floats ) 
--               to each point in a grid.
-- elemPerPoint: Number of values that currently define each point
--               in a grid, e.g. a point defined by an X and Y
--               coordinate with 3 floats as existing attributes
--               will pass a 5 here.
-- numX:         Number of columns in the existing grid.
-- numY:         Number of rows in the existing grid.
-- attribFxn:    A function of the form:
--                  columnNumber -> rowNumber -> ListOfAttributes
--               It is used to define unique attributes to
--               different grid points. 
-- grid:         The grid to add the elements to.
-- return:       Nothing: If the # of elements per point *
--                        number of columns *
--                        number of rows 
--                        is not equal to the length of the 
--                        provided grid.
--               Just [Float] = if successful
--------------------------------------------------------------
addAttributesToGrid :: Word32 -> Word32 -> Word32 -> (Word32 -> Word32 -> [Float]) -> [Float] -> Maybe [Float]
addAttributesToGrid elemPerPoint numX numY attribFxn grid
   | elemPerPoint * numX * numY /= fromIntegral (length grid) = Nothing
   | otherwise = Just ( applyAttributes (fromIntegral elemPerPoint) numX 0 0 attribFxn grid )
   where applyAttributes _       _     _     _     _          [] = []
         applyAttributes _elemPerPoint _numX _curX _curY _attribFxn _grid
            | ( _curX == _numX ) = applyAttributes _elemPerPoint _numX 0 (_curY + 1) _attribFxn _grid
            | otherwise = ( take _elemPerPoint _grid )      ++
                          ( _attribFxn _curX _curY ) ++
                          applyAttributes _elemPerPoint _numX (_curX + 1) _curY
                             _attribFxn (drop _elemPerPoint _grid)


--------------------------------------------------------------
-- printGrid 
-- Description:  Debug function that helps visualize a grid
--               by printing it in an orderly fashion. 
-- grid:         The grid to print
-- elemPerPoint: Number of values that currently define each point
--               in a grid, e.g. a point defined by an X and Y
--               coordinate with 3 floats as existing attributes
--               will pass a 5 here.
-- return:       IO () 
--------------------------------------------------------------
printGrid :: (Num a, Show a) => [a] -> Word32 -> IO ()
printGrid []   _      = return ()
printGrid grid elemPerPoint = do
   putStrLn $ show ( take elemPerPointInt grid )
   printGrid ( drop elemPerPointInt grid ) elemPerPoint
   where elemPerPointInt = ( fromIntegral elemPerPoint ) :: Int


--------------------------------------------------------------
-- createIndicesArray2D
-- Description: Defines a trianges for cells in a 2D grid
--              ( a cell here means the area bounded by 4
--              adjacent grid pointsthat make a square ). Uses
--              the top left point of each cell to create 
--              the triangles. 
-- numX:        number of points in each row of the 2D grid.
-- numY:        number of points in each column of the 2D grid.
-- return:      A list of indexes representing the triangles for 
--              the grid. The indexes are into the X,Y
--              pairs for the grid.
--              E.g. for the 3x2 grid (numX = 3, numY = 2):
--                  [-0.1, 0.9,  // 1st X,Y coordinate / column 0 / row 0 / index 0
--                    0.0, 0.9,  // 2nd X,Y coordinate / column 1 / row 0 / index 1
--                    0.1, 0.9,  // 3rd X,Y coordinate / column 2 / row 0 / index 2
--                   -0.1, 0.7,  // 4th X,Y coordinate / column 0 / row 1 / index 3
--                    0.0, 0.7,  // 5th X,Y coordinate / column 1 / row 1 / index 4
--                    0.1, 0.7]  // 6th X,Y coordinate / column 2 / row 1 / index 5
--                 
--                   * the index of 3 represents the (-0.1, 0.7)
--                   * The return value would be
--                      [0,3,4   // left triangle for the first cell (1st, 2nd, 5th, 4th coordinates)
--                       0,1,4   // right triangle for the first cell (1st, 2nd, 5th, 4rd coordinates)
--                       1,4,5   // left triangle for second cell (2nd, 3rd, 6th, 5th coordinates)
--                       1,2,5   // right triangle for second cell (2nd, 3rd, 6th, 5th coordinates)
--------------------------------------------------------------
createIndicesArray2D :: Word32 -> Word32 -> [Word32]
createIndicesArray2D numX numY
   | numX < 2 = [] 
   | numY < 2 = []
   | otherwise = _createIndicesArrayInner2D numX numY 0 0



--------------------------------------------------------------
-- combineIndicesArrays2D 
-- Description:  Creates a single element buffer object (EBO) indices list
--               given multiple vertex arrays and their corresponding
--               indices lists.
-- iArrList:     List of triangle indices lists. The position of each
--               sublist withing the superlist must be the same of
--               the corresponding vertex array within the vArrList
--               superlist
-- vArrList:     List of 2D coordinate lists.
-- elemPerPoint: Number of floating point values that are used to
--               identify a point and it's attributes. E.g. a 2D grid
--               grid in which each grid point has an x-coordinate,
--               a y-coordinate, and 3 floating point values to define
--               the colour would have a elemPerPoint of 5.
-- return:       The combined EBO list, e.g. if provided
--               iArrList = [ idxList1, idxList2, idxList3 ]
--               vArrList = [ vtxArr1,  vtxArr2,  vtxArr3  ]
--               will return the appropriate indices list for
--               vtxArr1 ++ vtxArr2 ++ vtxArr3
--------------------------------------------------------------
combineIndicesArrays2D :: [[Word32]] -> [[Float]] -> Word32 -> Maybe [Word32]
combineIndicesArrays2D iArrList vArrList elemPerPoint
   | (length vArrList) /= (length iArrList) = Nothing
   | ( elemPerPoint == 0 ) = Nothing
   | otherwise = Just ( combineIndArr iArrList vArrList [] 0 (fromIntegral elemPerPoint) )
   where combineIndArr [] _  iAccumList _ _ = iAccumList
         combineIndArr _  [] iAccumList _ _ = iAccumList
         combineIndArr (i:is) (v:vs) iAccumList vLenSum elemPerPoint' =
            combineIndArr is
                          vs
                          ( iAccumList ++ ( map (\x -> fromIntegral (vLenSum `div` elemPerPoint') + x ) i ) )
                          ( vLenSum + ( length v ) )
                          elemPerPoint'





