module TextLibrary where

import Data.Word
import DrawLibrary


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
---------------------------    Character Maps     -----------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

shade_A = [ [ 0, 1, 1, 1, 0, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 1, 1, 1, 1, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 0, 0, 0, 1, 0 ] ]
 
shade_B = [ [ 1, 1, 1, 1, 0, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 1, 1, 1, 1, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 1, 1, 1, 0, 0 ] ]

shade_C = [ [ 0, 1, 1, 1, 1, 0 ],
            [ 1, 0, 0, 0, 0, 0 ],
            [ 1, 0, 0, 0, 0, 0 ],
            [ 1, 0, 0, 0, 0, 0 ],
            [ 0, 1, 1, 1, 1, 0 ] ]

shade_D = [ [ 1, 1, 1, 1, 0, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 1, 1, 1, 0, 0 ] ]

shade_E = [ [ 1, 1, 1, 1, 1, 0 ],
            [ 1, 0, 0, 0, 0, 0 ],
            [ 1, 1, 1, 1, 1, 0 ],
            [ 1, 0, 0, 0, 0, 0 ],
            [ 1, 1, 1, 1, 1, 0 ] ]

shade_F = [ [ 1, 1, 1, 1, 1, 0 ],
            [ 1, 0, 0, 0, 0, 0 ],
            [ 1, 1, 1, 1, 1, 0 ],
            [ 1, 0, 0, 0, 0, 0 ],
            [ 1, 0, 0, 0, 0, 0 ] ]

shade_G = [ [ 1, 1, 1, 1, 1, 0 ],
            [ 1, 0, 0, 0, 0, 0 ],
            [ 1, 0, 1, 1, 1, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 1, 1, 1, 0, 0 ] ]

shade_H = [ [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 1, 1, 1, 1, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 0, 0, 0, 1, 0 ] ]

shade_I = [ [ 1, 1, 1, 1, 1, 0 ],
            [ 0, 0, 1, 0, 0, 0 ],
            [ 0, 0, 1, 0, 0, 0 ],
            [ 0, 0, 1, 0, 0, 0 ],
            [ 1, 1, 1, 1, 1, 0 ] ]

shade_J = [ [ 0, 0, 0, 0, 1, 0 ],
            [ 0, 0, 0, 0, 1, 0 ],
            [ 0, 0, 0, 0, 1, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 0, 1, 1, 1, 0, 0 ] ]

shade_K = [ [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 0, 0, 1, 0, 0 ],
            [ 1, 1, 1, 0, 0, 0 ],
            [ 1, 0, 0, 1, 0, 0 ],
            [ 1, 0, 0, 0, 1, 0 ] ]

shade_L = [ [ 1, 0, 0, 0, 0, 0 ],
            [ 1, 0, 0, 0, 0, 0 ],
            [ 1, 0, 0, 0, 0, 0 ],
            [ 1, 0, 0, 0, 0, 0 ],
            [ 1, 1, 1, 1, 1, 0 ] ]

shade_M = [ [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 1, 0, 1, 1, 0 ],
            [ 1, 0, 1, 0, 1, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 0, 0, 0, 1, 0 ] ]

shade_N = [ [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 1, 0, 0, 1, 0 ],
            [ 1, 0, 1, 0, 1, 0 ],
            [ 1, 0, 0, 1, 1, 0 ],
            [ 1, 0, 0, 0, 1, 0 ] ]

shade_O = [ [ 0, 1, 1, 1, 0, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 0, 1, 1, 1, 0, 0 ] ]

shade_P = [ [ 1, 1, 1, 1, 0, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 1, 1, 1, 0, 0 ],
            [ 1, 0, 0, 0, 0, 0 ],
            [ 1, 0, 0, 0, 0, 0 ] ]

shade_Q = [ [ 0, 1, 1, 1, 0, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 0, 0, 1, 0, 0 ],
            [ 0, 1, 1, 0, 1, 0 ] ]

shade_R = [ [ 1, 1, 1, 1, 0, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 1, 1, 1, 0, 0 ],
            [ 1, 0, 0, 1, 0, 0 ],
            [ 1, 0, 0, 0, 1, 0 ] ]

shade_S = [ [ 0, 1, 1, 1, 1, 0 ],
            [ 1, 0, 0, 0, 0, 0 ],
            [ 0, 1, 1, 1, 0, 0 ],
            [ 0, 0, 0, 0, 1, 0 ],
            [ 1, 1, 1, 1, 0, 0 ] ]

shade_T = [ [ 1, 1, 1, 1, 1, 0 ],
            [ 0, 0, 1, 0, 0, 0 ],
            [ 0, 0, 1, 0, 0, 0 ],
            [ 0, 0, 1, 0, 0, 0 ],
            [ 0, 0, 1, 0, 0, 0 ] ]

shade_U = [ [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 0, 1, 1, 1, 0, 0 ] ]

shade_V = [ [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 0, 1, 0, 1, 0, 0 ],
            [ 0, 0, 1, 0, 0, 0 ] ]

shade_W = [ [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 0, 1, 0, 1, 0 ],
            [ 0, 1, 0, 1, 0, 0 ] ]

shade_X = [ [ 1, 0, 0, 0, 1, 0 ],
            [ 0, 1, 0, 1, 0, 0 ],
            [ 0, 0, 1, 0, 0, 0 ],
            [ 0, 1, 0, 1, 0, 0 ],
            [ 1, 0, 0, 0, 1, 0 ] ]

shade_Y = [ [ 1, 0, 0, 0, 1, 0 ],
            [ 0, 1, 0, 1, 0, 0 ],
            [ 0, 0, 1, 0, 0, 0 ],
            [ 0, 0, 1, 0, 0, 0 ],
            [ 0, 0, 1, 0, 0, 0 ] ]

shade_Z = [ [ 1, 1, 1, 1, 1, 0 ],
            [ 0, 0, 0, 1, 0, 0 ],
            [ 0, 0, 1, 0, 0, 0 ],
            [ 0, 1, 0, 0, 0, 0 ],
            [ 1, 1, 1, 1, 1, 0 ] ]

shade_1 = [ [ 0, 0, 1, 0, 0, 0 ],
            [ 0, 1, 1, 0, 0, 0 ],
            [ 0, 0, 1, 0, 0, 0 ],
            [ 0, 0, 1, 0, 0, 0 ],
            [ 0, 0, 1, 0, 0, 0 ] ]

shade_2 = [ [ 1, 1, 1, 1, 0, 0 ],
            [ 0, 0, 0, 0, 1, 0 ],
            [ 0, 1, 1, 1, 0, 0 ],
            [ 1, 0, 0, 0, 0, 0 ],
            [ 1, 1, 1, 1, 1, 0 ] ]

shade_3 = [ [ 1, 1, 1, 1, 0, 0 ],
            [ 0, 0, 0, 0, 1, 0 ],
            [ 0, 1, 1, 1, 0, 0 ],
            [ 0, 0, 0, 0, 1, 0 ],
            [ 1, 1, 1, 1, 0, 0 ] ]

shade_4 = [ [ 1, 0, 0, 0, 1, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 0, 1, 1, 1, 1, 0 ],
            [ 0, 0, 0, 0, 1, 0 ],
            [ 0, 0, 0, 0, 1, 0 ] ]

shade_5 = [ [ 1, 1, 1, 1, 1, 0 ],
            [ 1, 0, 0, 0, 0, 0 ],
            [ 1, 1, 1, 1, 0, 0 ],
            [ 0, 0, 0, 0, 1, 0 ],
            [ 1, 1, 1, 1, 0, 0 ] ]

shade_6 = [ [ 0, 1, 1, 1, 0, 0 ],
            [ 1, 0, 0, 0, 0, 0 ],
            [ 1, 1, 1, 1, 0, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 0, 1, 1, 1, 0, 0 ] ]

shade_7 = [ [ 1, 1, 1, 1, 1, 0 ],
            [ 0, 0, 0, 1, 0, 0 ],
            [ 0, 0, 1, 0, 0, 0 ],
            [ 0, 1, 0, 0, 0, 0 ],
            [ 1, 0, 0, 0, 0, 0 ] ]

shade_8 = [ [ 0, 1, 1, 1, 0, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 0, 1, 1, 1, 0, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 0, 1, 1, 1, 0, 0 ] ]

shade_9 = [ [ 0, 1, 1, 1, 0, 0 ],
            [ 1, 0, 0, 0, 1, 0 ],
            [ 0, 1, 1, 1, 1, 0 ],
            [ 0, 0, 0, 0, 1, 0 ],
            [ 0, 1, 1, 1, 0, 0 ] ]

shade_ze = [ [ 0, 1, 1, 1, 0, 0 ],
             [ 1, 0, 0, 1, 1, 0 ],
             [ 1, 0, 1, 0, 1, 0 ],
             [ 1, 1, 0, 0, 1, 0 ],
             [ 0, 1, 1, 1, 0, 0 ] ]

shade_sp = [ [ 0, 0, 0, 0, 0, 0 ],
             [ 0, 0, 0, 0, 0, 0 ],
             [ 0, 0, 0, 0, 0, 0 ],
             [ 0, 0, 0, 0, 0, 0 ],
             [ 0, 0, 0, 0, 0, 0 ] ]

shade_err = [ [ 1, 1, 1, 1, 1, 0 ],
              [ 1, 1, 1, 1, 1, 0 ],
              [ 1, 1, 1, 1, 1, 0 ],
              [ 1, 1, 1, 1, 1, 0 ],
              [ 1, 1, 1, 1, 1, 0 ] ]




-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
----------------------   Internal Untility Functions   ------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

--------------------------------------------------------------
-- _getCharMap 
-- Description: Returns the character map for the provided 
--              character. If a character doesn't have a
--              map, an error character is returned.
-- char:        Input character
-- return:      A character map 
--------------------------------------------------------------
_getCharMap :: Char -> [[Int]]
_getCharMap char
   | ( char == 'a' ) || ( char == 'A' ) = shade_A
   | ( char == 'b' ) || ( char == 'B' ) = shade_B
   | ( char == 'c' ) || ( char == 'C' ) = shade_C
   | ( char == 'd' ) || ( char == 'D' ) = shade_D
   | ( char == 'e' ) || ( char == 'E' ) = shade_E
   | ( char == 'f' ) || ( char == 'F' ) = shade_F
   | ( char == 'g' ) || ( char == 'G' ) = shade_G
   | ( char == 'h' ) || ( char == 'H' ) = shade_H
   | ( char == 'i' ) || ( char == 'I' ) = shade_I
   | ( char == 'j' ) || ( char == 'J' ) = shade_J
   | ( char == 'k' ) || ( char == 'K' ) = shade_K
   | ( char == 'l' ) || ( char == 'L' ) = shade_L
   | ( char == 'm' ) || ( char == 'M' ) = shade_M
   | ( char == 'n' ) || ( char == 'N' ) = shade_N
   | ( char == 'o' ) || ( char == 'O' ) = shade_O
   | ( char == 'p' ) || ( char == 'P' ) = shade_P
   | ( char == 'q' ) || ( char == 'Q' ) = shade_Q
   | ( char == 'r' ) || ( char == 'R' ) = shade_R
   | ( char == 's' ) || ( char == 'S' ) = shade_S
   | ( char == 't' ) || ( char == 'T' ) = shade_T
   | ( char == 'u' ) || ( char == 'U' ) = shade_U
   | ( char == 'v' ) || ( char == 'V' ) = shade_V
   | ( char == 'w' ) || ( char == 'W' ) = shade_W
   | ( char == 'x' ) || ( char == 'X' ) = shade_X
   | ( char == 'y' ) || ( char == 'Y' ) = shade_Y
   | ( char == 'z' ) || ( char == 'Z' ) = shade_Z
   | ( char == '1' )                    = shade_1
   | ( char == '2' )                    = shade_2
   | ( char == '3' )                    = shade_3
   | ( char == '4' )                    = shade_4
   | ( char == '5' )                    = shade_5
   | ( char == '6' )                    = shade_6
   | ( char == '7' )                    = shade_7
   | ( char == '8' )                    = shade_8
   | ( char == '9' )                    = shade_9
   | ( char == '0' )                    = shade_ze
   | ( char == ' ' )                    = shade_sp
   | otherwise                          = shade_err




--------------------------------------------------------------
-- _getCharPoints 
-- Description: Creates a set of coordinates (points) for a 2D  
--              character based on a character map. Also
--              will add 4 floats representing the rgba
--              colours provided to each x,y coordinate.
-- topLeftX:    top-left x coordinates of the character 
-- topLeftY:    top-left y coordinates of the character
-- spacingX:    the width of each "pixel" that makes up the
--              character. A "pixel" is a 0 or 1 in the 
--              character map.
-- spacingY:    the height of each "pixel" that makes up the
--              character. A "pixel" is a 0 or 1 in the 
--              character map.
-- inRGBA:      List of 4 floating point values that defines
--              the character's colour. 
-- charMap:     The character map to generate points for.
-- return:      The points for the character. Each point is
--              defined as follows. e.g.
--              point 1: x coordinate, y coordinate, red colour,
--                       green colour, blue colour, alpha
--                       transparancy value
--------------------------------------------------------------
_getCharPoints :: Float -> Float -> Float -> Float -> [Float] -> [[Int]] -> [Float]
_getCharPoints topLeftX topLeftY spacingX spacingY inRGBA charMap =
   case charMap of
      []        -> []
      (row:map) -> ( processMapRow topLeftX topLeftY spacingX spacingY inRGBA row ) ++
                      ( _getCharPoints topLeftX (topLeftY - spacingY) spacingX spacingY inRGBA map )
         -- a character map is a list of lists. Process each interior list one at a time.
   where processMapRow topLeftX' topLeftY' spacingX' spacingY' colour charMapRow =
            case charMapRow of
               []     -> []
               (x:xs) -> case x of
                            -- move on to the next character map value if the current one is zero
                            0 -> processMapRow (topLeftX' + spacingX') topLeftY' 
                                    spacingX' spacingY' colour xs
                            -- define the coordinate and colour information for the current character
                            -- map value if it is 1, then continue to the next.
                            1 -> ( makeVisible topLeftX' topLeftY' spacingX' spacingY' colour ) ++
                                    ( processMapRow (topLeftX' + spacingX) topLeftY' 
                                       spacingX' spacingY' colour xs )
         -- make visible means essentially to define the 4 coordinates for a point and to add
         -- in the colour information.
         makeVisible topLeftX' topLeftY' spacingX' spacingY' colour =
            [ topLeftX'           , topLeftY'            ] ++ colour ++  -- triangle 1, point 1 
            [ topLeftX' + spacingX, topLeftY'            ] ++ colour ++  -- triangle 1, point 2
            [ topLeftX'           , topLeftY' - spacingY ] ++ colour ++  -- triangle 1, point 3
            [ topLeftX'           , topLeftY' - spacingY ] ++ colour ++  -- triangle 2, point 1
            [ topLeftX' + spacingX, topLeftY'            ] ++ colour ++  -- triangle 2, point 2
            [ topLeftX' + spacingX, topLeftY' - spacingY ] ++ colour     -- triangle 2, point 3
          




-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------     External Function      --------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------



--------------------------------------------------------------
-- makeStringPoints 
-- Description: Creates a list of points (x,y,r,g,b,a) for a 2D  
--              string. Generates both coordinate (x,y) and
--              colour (r,g,b,a) information. Strings are
--              generated from left to right.
-- inString:    the string to generate point information for 
-- topLeftX:    top-left x coordinates of the string 
-- topLeftY:    top-left y coordinates of the string
-- spacingX:    the width of each "pixel" that makes up each
--              character. A "pixel" is a 0 or 1 in the 
--              character map.
-- spacingY:    the height of each "pixel" that makes up each
--              character. A "pixel" is a 0 or 1 in the 
--              character map.
-- colourFxn:   A function that takes an integer (starting at
--              zero ) and generates a list of 4 floating
--              point values that defines that define a
--              character's colour. e.g.
--              colour of 1st char = colourFxn 0
--              colour of 2nd char = colourFxn 1
-- return:      The points for the string. Each point is
--              defined as follows. e.g.
--              point 1: x coordinate, y coordinate, red colour,
--                       green colour, blue colour, alpha
--                       transparancy value
--------------------------------------------------------------
makeStringPoints :: String -> Float -> Float -> Float -> Float -> ( Word32 -> [Float] ) -> [Float] 
makeStringPoints inString topLeftX topLeftY spacingX spacingY colourFxn = 
   mspInternal inString topLeftX topLeftY spacingX spacingY colourFxn 0
         -- nextTopLeftX is used to move from character to character. Since strings are drawn
         -- from left to right only, it only moves the topLeftX value.
   where nextTopLeftX topLeftX' spacingX' curChar =
            topLeftX' + ( spacingX' * ( fromIntegral ( length $ (_getCharMap curChar)!!0 ) :: Float ) )
         -- mspInternal is a recursive function that defines the points for each character.
         mspInternal _inString topLeftX' topLeftY' spacingX' spacingY' _colourFxn counter =
            case _inString of
               []       -> []
               -- _getCharPoints gets points for the current character
               char:str -> _getCharPoints topLeftX'
                                          topLeftY'
                                          spacingX'
                                          spacingY'
                                          (_colourFxn counter)
                                          (_getCharMap char) ++
                           -- append the current character to recursive call of mspInternal for
                           -- the remaining characters.
                           mspInternal str 
                                       (nextTopLeftX topLeftX' spacingX' char)
                                       topLeftY'
                                       spacingX'
                                       spacingY'
                                       _colourFxn
                                       (counter + 1)



