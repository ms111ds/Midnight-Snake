module Main where

-- Haskell functionality libraries
--import Control.Monad (when)
--import Control.Exception (bracket)
import Foreign
import Foreign.C.String ( newCAStringLen, castCCharToChar, withCAStringLen, withCString )
import System.IO
import System.Exit
import qualified Data.Vector.Storable as VS
import Data.IORef

-- Math Libraries
--import Linear
--import Data.Fixed
--import qualified Data.Set as S

-- Windowing Libraries
import qualified Graphics.UI.GLFW as GLFW

-- OpenGL Libraries
import Graphics.GL.Core46
--import Graphics.GL.Types

-- Picture Libraries
import Codec.Picture ( readImage, generateImage, convertRGBA8, DynamicImage(..),
       Image(..), PixelRGBA8(..) )

-- Draw Libraries
import DrawLibrary
import TextLibrary


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
---------------------------     Shader Files      -----------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
bg_vertex_shader_path :: String
bg_vertex_shader_path = "app/Shaders/bg_vertex_shader_file"

bg_fragment_shader_path :: String
bg_fragment_shader_path = "app/Shaders/bg_fragment_shader_file"

icon_vertex_shader_path :: String
icon_vertex_shader_path = "app/Shaders/snake_vertex_shader_file"

icon_fragment_shader_path :: String
icon_fragment_shader_path = "app/Shaders/snake_fragment_shader_file"

text_vertex_shader_path :: String
text_vertex_shader_path = "app/Shaders/text_vertex_shader_file"

text_fragment_shader_path :: String
text_fragment_shader_path = "app/Shaders/text_fragment_shader_file"


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
---------------------------      Media Files      -----------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
snake_pic_path :: String
snake_pic_path = "pictures/snake_pic.png"



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--------------------  Window and Game "Picture Element" -----------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Window dimensions in screen pixels
window_x :: Int
window_x = 1024

window_y :: Int
window_y = 768

-- game title
window_title :: String
window_title = "Midnight Snake"

-- Game picture element: this is the smallest size of any in-game units.
-- e.g. if 16, the snake should move 16 screen pixels at at time
game_pixel_len :: Int
game_pixel_len = 16 

-- normalized sizes of our smallest in game unit "game_pixel_len". E.g. if game_pixel_len
-- is equal to 16, then a movement unit_x in the positive x direction equates to a movement of 16
-- screen pixels in the positive x direction.
-- The "2" comes from the fact that normalized screen coordinates are between -1 and 1
-- (the distance between these two is 2)
unit_x :: Float
unit_x = ( fromIntegral ( 2 * game_pixel_len ) :: Float ) / ( fromIntegral window_x )

unit_y :: Float
unit_y = ( fromIntegral ( 2 * game_pixel_len ) :: Float ) / ( fromIntegral window_y )



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
------------------------      Custom Data Types     ---------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
data MemoryAlloc = AllocWindow   GLFW.Window
                 | AllocProgram  GLuint
                 | AllocFloatArr ( Ptr GLfloat )
                 | AllocUintArr  ( Ptr GLuint )
                 deriving (Show)

data DrawInfoBG = DrawInfoBG { bg_get_vao_id :: GLuint,                  -- vao id
                           bg_get_num_indices :: GLsizei,                -- number of indices
                           bg_get_prog_id :: GLuint,                     -- shader program id
                           bg_get_maybe_texture_id :: Maybe GLuint,      -- texture id (if available)
                           bg_get_maybe_float_data :: Maybe [Float],     -- extra float data (if available)
                           bg_get_maybe_int_data   :: Maybe [Integer],   -- extra integer data (if available)
                           bg_is_wireframe :: Bool                       -- if drawing in wireframe mode
                         } deriving (Show)

data DrawInfoTXT = DrawInfoTXT { txt_get_vao_id :: GLuint,                  -- vao id
                           txt_get_num_vertices :: GLsizei,               -- number of vertices
                           txt_get_prog_id :: GLuint,                     -- shader program id
                           txt_get_maybe_texture_id :: Maybe GLuint,      -- texture id (if available)
                           txt_get_maybe_float_data :: Maybe [Float],     -- extra float data (if available)
                           txt_get_maybe_int_data   :: Maybe [Integer],   -- extra integer data (if available)
                           txt_is_wireframe :: Bool,                      -- if drawing in wireframe mode
                           get_txt_data_ptr :: Ptr Float,
                           txt_get_vbo_id :: GLuint,
                           txt_get_topleft_x :: Float,
                           txt_get_topleft_y :: Float
                         } deriving (Show)

data LabeledDI = Background DrawInfoBG
               | Icon  DrawInfoBG
               | Text DrawInfoTXT 

data GameState = GameState { getScore :: Int,
                             getExtraLives :: Int
                           } deriving (Show)
                            


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--------------------------    User Input Functions    -------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


--------------------------------------------------------------
-- key_callback 
-- Description: Describes the actions taken when a key is
--              pressed by the user
-- last_key:    Mutable IORef object the contains the last key
--              pressed by the user
-- return:      IO ( Nothing )
--              
-- side effect: Either 1) the window will be marked to close
--                     2) last_key will be updated with up,
--                        down, left, or right
--                     3) No side effect if unused key pressed
--------------------------------------------------------------
key_callback :: IORef GLFW.Key -> GLFW.KeyCallback
key_callback last_key window key scan_code key_state modifier_keys = do
   putStrLn $ ( show key_state ) ++ " " ++ ( show key )
   case key_state of
      GLFW.KeyState'Pressed -> key_action last_key window key
      _                     -> return ()
   where key_action _last_key _window _key = case _key of
            GLFW.Key'Escape -> GLFW.setWindowShouldClose _window True
            GLFW.Key'Up     -> writeIORef _last_key GLFW.Key'Up
            GLFW.Key'Down   -> writeIORef _last_key GLFW.Key'Down
            GLFW.Key'Left   -> writeIORef _last_key GLFW.Key'Left
            GLFW.Key'Right  -> writeIORef _last_key GLFW.Key'Right
            GLFW.Key'W      -> writeIORef _last_key GLFW.Key'Up
            GLFW.Key'S      -> writeIORef _last_key GLFW.Key'Down
            GLFW.Key'A      -> writeIORef _last_key GLFW.Key'Left
            GLFW.Key'D      -> writeIORef _last_key GLFW.Key'Right
            _               -> return ()



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
----------------    Shader Loading And Compilation Functions    ---------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------



--------------------------------------------------------------
-- _compile_shader
-- Description: Internal function that compiles a shader. Used
--              by load_shader()
-- shader_id:   id of shader object
-- shader_code: OpenGL Shading Language code as a string
-- return:      Success: IO ( Nothing )
--              Fail:    IO ( Just error_message )
--------------------------------------------------------------
_compile_shader :: GLuint -> String -> IO (Maybe String)
_compile_shader shader_id shader_code = do

   -- load shader code into OpenGL
   withCAStringLen shader_code $ \(path_ptr, path_len) ->
      withArray [path_ptr] $ \path_ptr_array ->
         withArray [fromIntegral path_len :: GLint] $ \path_len_array ->
            glShaderSource shader_id 1 path_ptr_array path_len_array

   -- compile shader and get compilation status
   glCompileShader shader_id
   status <- alloca $ \status_ptr -> do
      glGetShaderiv shader_id GL_COMPILE_STATUS status_ptr
      (\s -> fromIntegral s :: GLboolean) <$> peek status_ptr 
                             
   -- if error in compilation, return errors 
   case status of
      GL_TRUE -> return Nothing
      _       -> do
         -- get error buffer length
         err_buf_len <- alloca $ \err_buf_len_ptr -> do
            glGetShaderiv shader_id GL_INFO_LOG_LENGTH err_buf_len_ptr 
            peek err_buf_len_ptr

         -- get error message
         err_string <- alloca $ \log_len_ptr ->
            allocaBytes ( fromIntegral err_buf_len ) $ \log_ptr -> do
               glGetShaderInfoLog shader_id ( fromIntegral err_buf_len )
                                  log_len_ptr log_ptr
               log_len <- ( fromIntegral <$> peek log_len_ptr  )
               peekArray ( log_len :: Int ) log_ptr

         return $ Just ( map castCCharToChar err_string )
  

--------------------------------------------------------------
-- load_shader
-- Description: Creates a new ready-to-use shader object. Shader
--              code is compiled.
-- shader_type: Enum for type of shader to create , e.g.
--              GL_VERTEX_SHADER, 
--              GL_GEOMETRY_SHADER,
--              GL_FRAGMENT_SHADER
-- shader_path: path of file containing OpenGL Shading Language
--              code
-- return:      Success: IO ( Just shader_id )
--              Fail:    IO ( Nothing )
--------------------------------------------------------------
load_shader :: GLenum -> String -> IO ( Maybe GLuint )
load_shader shader_type shader_path = do

   -- create shader
   shader_id <- glCreateShader shader_type 
   maybe_err <- case shader_id of
                   0 -> return ( Just "Shader creation failed. Incorrect shader type" )
                   _ -> do
                      code <- withFile shader_path ReadMode $ \handle -> do
                         shader_code <- hGetContents' handle
                         return shader_code
                      _compile_shader shader_id code

   -- If compilation successful, return the shader id.
   -- Otherwise, print the error message, delete the shader object, and return nothing
   case maybe_err of
      (Just error_msg) -> do

         -- print error message
         let err_type_string = case shader_type of
               GL_VERTEX_SHADER   -> "Vertex shader compilation: "
               GL_GEOMETRY_SHADER -> "Geometry shader compilation: "
               GL_FRAGMENT_SHADER -> "Fragment shader compilation: "
               _                  -> "Other error: "
         let full_error_msg = "Shader error. " ++ err_type_string ++ error_msg
         putStrLn full_error_msg

         -- delete shader
         case shader_id of
            0  -> return ()
            valid_id -> glDeleteShader valid_id

         return Nothing
               
      _  -> return ( Just shader_id )



--------------------------------------------------------------
-- _link_shader_program 
-- Description: Internal function that links together shaders
--              into a shader program.
-- program id:  Shader program ID 
-- vertex_id:   Vertex shader ID
-- geometry_id: Geometry shader ID
-- fragment_id: Fragment shader ID 
-- return:      Success: IO ( Just program_id )
--              Fail:    IO ( Nothing )
--------------------------------------------------------------
_link_shader_program :: GLuint -> Maybe GLuint -> Maybe GLuint -> Maybe GLuint -> IO (Maybe GLuint)
_link_shader_program program_id vertex_id geometry_id fragment_id = do
   -- attach shader to program if available and link it
   case vertex_id of
      (Just v_id) -> glAttachShader program_id v_id
      _           -> return ()
   case geometry_id of
      (Just g_id) -> glAttachShader program_id g_id
      _           -> return ()
   case fragment_id of
      (Just f_id) -> glAttachShader program_id f_id
      _           -> return ()

   glLinkProgram program_id

   -- check for any errors in linking
   link_status <- alloca $ \link_status_ptr -> do
      glGetProgramiv program_id GL_LINK_STATUS link_status_ptr
      peek link_status_ptr

   -- return program ID if linking successful, otherwise print error
   if link_status == ( fromIntegral GL_TRUE :: GLint )
      then return ( Just program_id )
      else do
         -- print error message
         -- will need to get from OpenGL 1) size of error buffer
         --                              2) size of the error message
         --                              3) the error message itself 
         error_buf_len <- alloca $ \error_buf_len_ptr -> do
            glGetProgramiv program_id GL_INFO_LOG_LENGTH error_buf_len_ptr
            peek error_buf_len_ptr
         link_log <- alloca $ \log_len_ptr ->
            allocaBytes ( fromIntegral error_buf_len ) $ \log_ptr -> do
               glGetProgramInfoLog program_id ( fromIntegral error_buf_len )
                  log_len_ptr log_ptr
               log_len <- fromIntegral <$> peek log_len_ptr
               peekArray log_len log_ptr
         putStrLn $ "Shader Program Link Error: " ++ ( map castCCharToChar link_log )
         return Nothing



--------------------------------------------------------------
-- create_shader_program 
-- Description: Returns a shader program id with all the provided
--              shaders compiled and linked.
-- vertex_prog_path:   File path to vertex shader
-- geometry_prog_path: File path to geometry shader (optional)
-- fragment_prog_path: file path to fragment shader 
-- return:      Success: IO ( Just program_id )
--              Fail:    IO ( Nothing )
--------------------------------------------------------------
create_shader_program :: Maybe String -> Maybe String -> Maybe String -> IO ( Maybe GLuint )
create_shader_program vertex_prog_path geometry_prog_path fragment_prog_path = do 
      -- load the vertex shader from a file and compile it
      maybe_v_id    <- case vertex_prog_path of
                          Just path -> load_shader GL_VERTEX_SHADER path
                          _         -> return Nothing
      -- load the optional geometry shader from a file (if provided) and compile it
      either_m_g_id <- case geometry_prog_path of
                          Just path -> Left <$> ( load_shader GL_GEOMETRY_SHADER path )
                          _         -> return ( Right Nothing )
      -- load the fragment shader from a file and compile it
      maybe_f_id    <- case fragment_prog_path of 
                          Just path -> load_shader GL_FRAGMENT_SHADER path
                          _         -> return Nothing
      -- check if all provided shaders loaded/compiled correctly and if
      -- both vertex and fragment shaders were provided
      loading_ok  <- return ( was_shader_load_ok maybe_v_id either_m_g_id maybe_f_id )

      -- simplify geometry shader value.
      -- Just id: if geometry shader provided and compiled successfully
      -- Nothing: if geometry shader not provided or failed to compile
      maybe_g_id    <- case either_m_g_id of
                          Left maybe_id -> return maybe_id
                          _             -> return Nothing

      -- create shader program if everything OK up to this point
      maybe_prog_id <- case loading_ok of
                        True -> Just <$> glCreateProgram
                        _    -> return Nothing

      -- link shader program if overything OK up to this point, but
      -- delete the program if linking fails
      prog_id <- case (maybe_prog_id) of
                    Nothing -> return Nothing -- Case where a shader did not load correctly
                                              -- or the Vertex or Fragment shader is missing
                    Just 0  -> return Nothing -- Case where shader program creation failed
                    Just unlinked_id -> do     
                       link_prog_id <- _link_shader_program unlinked_id maybe_v_id maybe_g_id maybe_f_id
                       case link_prog_id of
                          (Just linked_id) -> return (Just linked_id)
                          _          -> do
                             glDeleteProgram unlinked_id
                             return Nothing
      -- delete the individual shaders
      delete_shader maybe_v_id
      delete_shader maybe_g_id
      delete_shader maybe_f_id
      return prog_id
   where was_shader_load_ok m_vid em_gid m_fid
            -- Shader program compilation should fail if any of the following are true:
            -- 1) Either the vertex or fragment shaders were not provided
            -- 2) Any shader failed to compile load/compile
            | m_vid   == Nothing = False
            | em_gid  == (Left Nothing :: Either (Maybe GLuint) (Maybe GLuint) ) = False
            | m_fid   == Nothing = False
            | otherwise          = True
         delete_shader maybe_shdr_id = case maybe_shdr_id of
            ( Just shader_id ) -> glDeleteShader shader_id
            _           -> return ()
              
 
   
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-----------------------    Texture Loading Functions    -----------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------



--------------------------------------------------------------
-- load_picture 
-- Description: Loads an image from a file. The image must 
--              have an alpha channel, e.g. .png.
-- pic_path:    File path to the image.
-- return:      Success: IO ( Just (Image PixelRGBA8) )
--              Fail:    IO ( Nothing )
--------------------------------------------------------------
load_picture :: String -> IO ( Maybe ( Image PixelRGBA8 ) )
load_picture pic_path = do
   -- load image from file as a dynamic image
   eitherPic <- readImage pic_path 
   maybe_dyn_img <- case eitherPic of
                Left err -> do
                   putStrLn err
                   return Nothing
                Right di -> return (Just di)
   -- return either nothing or the image data
   case maybe_dyn_img of
      Nothing      -> return Nothing
      Just dyn_img -> return $ Just ( convertRGBA8 dyn_img )



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--------------    Program Exit and Resource Cleanup Functions    --------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


--------------------------------------------------------------
-- cleanup_objects
-- Description: Runs the destructor for all objects in an
--              MemoryAlloc list.
-- object_list: list of all object instances that need to be
--              destroyed.
-- return:      No return value, i.e. IO () 
--------------------------------------------------------------
cleanup_objects :: [MemoryAlloc] -> IO () 
cleanup_objects [] = return ()
cleanup_objects (object:remaining_objects) = do
   case object of
      ( AllocWindow w )  -> do
         GLFW.destroyWindow w
         putStrLn "Window Destroyed"
      ( AllocProgram p ) -> do
         glDeleteProgram p
         putStrLn "Program Deleted"
      ( AllocUintArr ptr ) -> do
         free ptr
         putStrLn "Unsigned Integer Array Freed"
      ( AllocFloatArr ptr ) -> do
         free ptr
         putStrLn "Floating Point Number Array Freed"
     -- _                     -> do
     --    putStrLn "Invalid object that wasn't cleaned up"
     --    return ()
   cleanup_objects remaining_objects



--------------------------------------------------------------
-- error_exit
-- Description: Early termination of program to be called in
--              case of an error. Calls destructor for all
--              objects in provided MemoryAlloc list.
-- object_list: list of all object instances that need to be
--              destroyed.
-- return:      No return value, i.e. IO () 
--------------------------------------------------------------
error_exit :: [MemoryAlloc] -> IO ()
error_exit object_list = do cleanup_objects object_list
                            GLFW.terminate
                            exitFailure



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------    Game Utility Functions    ------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------



--------------------------------------------------------------
-- process_draw_list
-- Description: Draws to window buffer. OpenGL objects to draw
--              must have been defined beforehand and their
--              details placed into a LabeledDI object.
--              Before actually drawing to the window buffer,
--              a few more "how to draw" instructions are 
--              carried out, e.g. shader program selection
--              and wireframe mode selection.
-- start_time:  A time reference point in case a drawing
--              instruction needs it.
-- list:        A list of grouped draw info objects containing
--              the details of what to draw 
-- return:      No return value, i.e. IO () 
--------------------------------------------------------------
process_draw_list :: Double -> [LabeledDI] -> IO ()
process_draw_list _          []   = return ()
process_draw_list start_time (gdi:list) = do
   case gdi of
      (Background info) -> do
         -- Draw border 
         let vao_bg = bg_get_vao_id info 
         let num_indices_bg = bg_get_num_indices info
         let polygon_mode = case (bg_is_wireframe info) of
                               True  -> GL_LINE
                               False -> GL_FILL
         glUseProgram $ bg_get_prog_id info 
         glBindVertexArray vao_bg
         glLineWidth (3.0 :: GLfloat)
         glPolygonMode GL_FRONT_AND_BACK polygon_mode 
         glDrawElements GL_TRIANGLES num_indices_bg GL_UNSIGNED_INT nullPtr
         return ()
      (Icon       info) -> do 
         -- Draw snake icon 
         let vao_icon = bg_get_vao_id info 
         let num_indices_icon = bg_get_num_indices info
         let polygon_mode = case (bg_is_wireframe info) of
                               True  -> GL_LINE
                               False -> GL_FILL
         glUseProgram $ bg_get_prog_id info 
         glBindVertexArray vao_icon
         glPolygonMode GL_FRONT_AND_BACK polygon_mode 
         glDrawElements GL_TRIANGLES num_indices_icon GL_UNSIGNED_INT nullPtr
         return ()
      (Text       info) -> do 
         -- Draw text 
         let vao_text = txt_get_vao_id info 
         let num_vertices_text = txt_get_num_vertices info
         let polygon_mode = case (txt_is_wireframe info) of
                               True  -> GL_LINE
                               False -> GL_FILL
         glUseProgram $ txt_get_prog_id info 
         glBindVertexArray vao_text
         glPolygonMode GL_FRONT_AND_BACK polygon_mode 
         glDrawArrays GL_TRIANGLES 0 num_vertices_text
         return ()
   process_draw_list start_time list
   


--------------------------------------------------------------
-- update_lives 
-- Description: Returns a new game state with an updated
--              number of lives if the update condition has
--              been met. 
-- is_cond_met: True or False. Depending if the score update
--              condition has been met.
-- fxn:         A function describing how the score is to be
--              updated when the condition is met
-- state:       The current game state
-- return:      Returns a new game state if the update 
--              condition was met, otherwise returns a copy
--              of the current game state. 
--------------------------------------------------------------
update_lives :: Bool -> (Int -> Int) -> GameState -> GameState
update_lives is_cond_met fxn state = case is_cond_met of
   False -> state
   True  -> GameState ( getScore state ) ( fxn $ getExtraLives state )


--------------------------------------------------------------
-- update_score 
-- Description: Returns a new game state with an updated
--              score if the update condition has been met. 
-- is_cond_met: True or False. Depending if the score update
--              condition has been met.
-- fxn:         A function describing how the score is to be
--              updated when the condition is met
-- state:       The current game state
-- return:      Returns a new game state if the update 
--              condition was met, otherwise returns a copy
--              of the current game state. 
--------------------------------------------------------------
update_score :: Bool -> (Int -> Int) -> GameState -> GameState
update_score is_cond_met fxn state = case is_cond_met of
   False -> state
   True  -> GameState ( fxn $ getScore state ) ( getExtraLives state )


--------------------------------------------------------------
-- score_to_str 
-- Description: Returns the game score, provided as an
--              integer, as a 4 digit string (0 extended to
--              ensure exactly 4 digits)
-- score:       The game score
-- return:      Returns a 4 digit string of the provided game 
--              score. Numbers with more than 4 digits will be
--              truncated to the 4 smallest digits. Numbers
--              with less than 4 digits will be zero extended
--              to the left.
--------------------------------------------------------------
score_to_str :: Int -> String
score_to_str score = score_to_str_inner score 4 
   where score_to_str_inner score' remaining_digits = case remaining_digits of
            0 -> ""
            x -> score_to_str_inner ( score' `div` 10 )
                                    ( remaining_digits - 1 ) ++
                 ( show $ score' `mod` 10 ) 






update_draw_list :: Bool -> [LabeledDI] -> GameState -> IO ([LabeledDI])
update_draw_list  _          []         _  = return []
update_draw_list  is_update  (gdi:list) state =
   case gdi of
      (Background info) -> ( (:) (Background info) ) <$> (update_draw_list is_update list state)
      (Icon       info) -> ( (:) (Icon info) ) <$> (update_draw_list is_update list state) 
      (Text       info) -> do 

         let topleft_x = txt_get_topleft_x info
         let topleft_y = txt_get_topleft_y info

         -- create text vertices points
         let text_pixel_width = unit_x / 3
         let text_pixel_height = unit_y / 3

         let x_points = makeStringPoints ( "X" ++ ( show $ getExtraLives state ) )
                                         topleft_x
                                         topleft_y
                                         text_pixel_width
                                         text_pixel_height
                                         ( \_ -> [1.0, 1.0, 1.0, 1.0] )


         let score_text_x = topleft_x + (15 * unit_x)
         let score_text_y = topleft_y
         let score_text_points =
                 makeStringPoints ( "SCORE " ++ ( score_to_str $ getScore state ) )
                                  score_text_x
                                  score_text_y
                                  text_pixel_width
                                  text_pixel_height
                                  ( \_ -> [1.0, 1.0, 1.0, 1.0] )


         let v_text = x_points ++ score_text_points

         let v_text_size = fromIntegral $ sizeOf ( 0.0 :: GLfloat ) * ( length v_text )
         let v_text_ptr = get_txt_data_ptr info
         pokeArray v_text_ptr v_text

         -- switch to text vertex array object
         vao_text <- return ( txt_get_vao_id info )
         glBindVertexArray vao_text

         -- select text vertex buffer object
         vbo_text <- return ( txt_get_vbo_id info ) 

         -- write text vertices to the vertex buffer object
         glBindBuffer GL_ARRAY_BUFFER vbo_text
         glBufferData GL_ARRAY_BUFFER v_text_size v_text_ptr GL_STATIC_DRAW

         -- unbind text Vertex Array Object
         glBindVertexArray 0

         let new_text_draw_info = DrawInfoTXT vao_text
                                              ( txt_get_num_vertices info )
                                              ( txt_get_prog_id info )
                                              Nothing
                                              Nothing
                                              Nothing
                                              False
                                              v_text_ptr
                                              vbo_text
                                              topleft_x
                                              topleft_y

         ( (:) (Text new_text_draw_info) ) <$> (update_draw_list is_update list state)




game_loop :: GLFW.Window -> Double -> [LabeledDI] -> GameState -> IO ()
game_loop window start_time grouped_di_list game_state = do
   continueGame <- not <$> GLFW.windowShouldClose window
   case continueGame of
      False -> return ()
      _     -> do
         GLFW.pollEvents

         glClearColor 0.05 0.007 0.1 1.0
         glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

         -- Draw everything 
         process_draw_list start_time grouped_di_list 

         cur_time <- ( maybe 0.0 id ) <$> GLFW.getTime
         let delta_time = cur_time - start_time
         let move_time = 1.5 :: Double
         let is_update = ( delta_time >= move_time )
         let new_game_state = ( (update_lives is_update (\x -> case x of { 0 -> 3 ; x -> x - 1} ) ).
                                (update_score is_update (\x -> x + 1) ) ) game_state

         new_grouped_di_list <- update_draw_list is_update grouped_di_list new_game_state
         new_start_time <- case is_update of
                              True -> do
                                  GLFW.setTime 0.0
                                  return (0.0 :: Double)
                              False -> return start_time

         GLFW.swapBuffers window

         game_loop window new_start_time new_grouped_di_list new_game_state 



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
------------------------------   Main Function   ------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


main :: IO ()
main = do

   let object_list = [] -- will contain objects that will need to be cleaned up

   -- initialize GLFW windowing library
   glfw_status <- GLFW.init
   case glfw_status of
      True -> return ()
      _    -> do
         putStrLn "Failed to initialize GLFW library"
         error_exit object_list 

   -- create the window
   GLFW.windowHint ( GLFW.WindowHint'ContextVersionMajor 4 ) 
   GLFW.windowHint ( GLFW.WindowHint'ContextVersionMinor 6 ) 
   GLFW.windowHint ( GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core ) 
   GLFW.windowHint ( GLFW.WindowHint'Resizable False )

   maybe_window <- GLFW.createWindow window_x window_y window_title Nothing Nothing

   case maybe_window of
      Nothing -> do
         putStrLn "Failed to create window"
         error_exit object_list
      _      -> return ()

   window <- ( \(Just w) -> w ) <$> return ( maybe_window )
   object_list <- return [ ( AllocWindow window ) ]

   -- create an OpenGL context for our window the OpenGL context on our window
   -- the one we will be sending commands to
   GLFW.makeContextCurrent ( Just window )


   -- Tell OpenGL how much space within the window it has to render
   (frame_buf_x, frame_buf_y) <- GLFW.getFramebufferSize window
   glViewport 0 0 ( fromIntegral frame_buf_x ) ( fromIntegral frame_buf_y )



   -- generate texture buffers texture
   let num_texture_bufs = 1
   texture_id_array_ptr <- mallocArray num_texture_bufs :: IO (Ptr GLuint)
   glGenTextures (fromIntegral num_texture_bufs) texture_id_array_ptr 
   object_list <- return ( ( AllocUintArr texture_id_array_ptr ):object_list )


   -- load images to be used

   -- load snake image
   maybe_snake_img <- load_picture snake_pic_path
   case maybe_snake_img of
      Nothing -> do
         putStrLn "failed to load snake img"
         error_exit object_list 
      _       -> return ()

   snake_img <- return ( (\(Just x) -> x ) maybe_snake_img )

   let snake_img_width  = fromIntegral ( imageWidth snake_img )
       snake_img_height = fromIntegral ( imageHeight snake_img )
       snake_img_data   = imageData snake_img

   -- configure snake texture
   snake_texture <- peekElemOff texture_id_array_ptr 0
   glBindTexture GL_TEXTURE_2D snake_texture
   glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S (fromIntegral GL_REPEAT)
   glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T (fromIntegral GL_REPEAT)
   glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_NEAREST)
   glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_NEAREST)

   -- generate an openGL texture with the snake image
   VS.unsafeWith snake_img_data $ \ dataPtr ->
      glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_RGBA)
                   snake_img_width snake_img_height 0 (fromIntegral GL_RGBA)
                   GL_UNSIGNED_BYTE (castPtr dataPtr)

   -- generate mipmap for snake texture
   glGenerateMipmap GL_TEXTURE_2D

   -- unbind snake texture for the time being
   glBindTexture GL_TEXTURE_2D 0
   

   -- Compile background shader program 
   maybe_shader_prog <- create_shader_program ( Just bg_vertex_shader_path )
                                                Nothing
                                              ( Just bg_fragment_shader_path )
   case maybe_shader_prog of
      Nothing -> do
         putStrLn "Failed to create shader program"
         error_exit object_list 
      _       -> return ()

   bg_shader_prog <- ( \(Just p) -> p ) <$> return ( maybe_shader_prog )
   object_list <- return ( ( AllocProgram bg_shader_prog ):object_list )

   -- Use the background shader program
   glUseProgram bg_shader_prog


   -- generate vertex array objects (VAOs)
   let num_vaos = 3
   vao_ptr <- mallocArray num_vaos :: IO (Ptr GLuint)
   object_list <- return ( ( AllocUintArr vao_ptr ):object_list )
   glGenVertexArrays ( fromIntegral num_vaos :: GLint ) vao_ptr

   -- create vertex buffer objects (VBOs)
   let num_vbos = 3
   vbo_ptr <- mallocArray num_vbos :: IO (Ptr GLuint)
   object_list <- return ( ( AllocUintArr vbo_ptr ):object_list )
   glGenBuffers ( fromIntegral num_vbos :: GLint ) vbo_ptr

   -- Create element buffer objects (EBOs)
   let num_ebos = 2
   ebo_ptr <- mallocArray num_ebos :: IO (Ptr GLuint)
   object_list <- return ( ( AllocUintArr ebo_ptr ):object_list )
   glGenBuffers ( fromIntegral num_ebos :: GLint ) ebo_ptr



   -------------------------------------------------------------------
   -- Setup scaffolding vertex data
   -------------------------------------------------------------------

   -- define colour 
   let scaffold_colour = [0.06, 0.042, 0.6]


   -- top left scaffold
   let v_topleft_grid = ( createGrid2D (-1.0)
                                 (1.0)
                                 ( unit_x * 4 )
                                 ( unit_y * 4)
                                 2 
                                 4 ) :: [GLfloat]

   let maybe_topleft_scaffold =
          addAttributesToGrid 2 2 4 (\_ _ -> scaffold_colour) v_topleft_grid 

   case maybe_topleft_scaffold of
      Nothing -> do
         putStrLn "failed to add attributes: top left scaffold"
         error_exit object_list 
      _       -> return ()

   v_topleft_grid_plus_attrib <- ( \(Just p) -> p ) <$> return ( maybe_topleft_scaffold )

   --printGrid v_topleft_grid 2
   --printGrid v_topleft_grid_plus_attrib 5

   -- top right scaffold
   let v_topright_grid = ( createGrid2D (1.0 - ( unit_x * 4 ) )
                                 (1.0)
                                 ( unit_x * 4 )
                                 ( unit_y * 4)
                                 2 
                                 4 ) :: [GLfloat]

   let maybe_topright_scaffold =
          addAttributesToGrid 2 2 4 (\_ _ -> scaffold_colour) v_topright_grid 

   case maybe_topright_scaffold of
      Nothing -> do
         putStrLn "failed to add attributes: top right scaffold"
         error_exit object_list 
      _       -> return ()

   v_topright_grid_plus_attrib <- ( \(Just p) -> p ) <$> return ( maybe_topright_scaffold )



   -- top bottom left scaffold
   let v_bottomleft_grid = ( createGrid2D (-1.0)
                                 (1.0 - 3 * (unit_y * 4) )
                                 ( unit_x * 4 )
                                 ( unit_y * 4)
                                 3 
                                 10 ) :: [GLfloat]

   let maybe_bottomleft_scaffold =
          addAttributesToGrid 2 3 10 (\_ _ -> scaffold_colour) v_bottomleft_grid 

   case maybe_bottomleft_scaffold of
      Nothing -> do
         putStrLn "failed to add attributes: bottom left scaffold"
         error_exit object_list 
      _       -> return ()

   v_bottomleft_grid_plus_attrib <- ( \(Just p) -> p ) <$> return ( maybe_bottomleft_scaffold )



   -- top bottom right scaffold
   let v_bottomright_grid = ( createGrid2D (1.0 - 2 * ( unit_x * 4 ) )
                                 (1.0 - 3 * (unit_y * 4) )
                                 ( unit_x * 4 )
                                 ( unit_y * 4)
                                 3 
                                 10 ) :: [GLfloat]

   let maybe_bottomright_scaffold =
          addAttributesToGrid 2 3 10 (\_ _ -> scaffold_colour) v_bottomright_grid 
 
   case maybe_bottomright_scaffold of
      Nothing -> do
         putStrLn "failed to add attributes: bottom right scaffold"
         error_exit object_list 
      _       -> return ()

   v_bottomright_grid_plus_attrib <- ( \(Just p) -> p ) <$> return ( maybe_bottomright_scaffold )



   -- top bottom scaffold
   let v_bottom_grid = ( createGrid2D ((-1.0) + 2 * ( unit_x * 4 ) )
                                 ((-1.0) + (unit_y * 4) )
                                 ( unit_x * 4 )
                                 ( unit_y * 4)
                                 13 
                                 2 ) :: [GLfloat]

   let maybe_bottom_scaffold =
          addAttributesToGrid 2 13 2 (\_ _ -> scaffold_colour) v_bottom_grid 
 
   case maybe_bottom_scaffold of
      Nothing -> do
         putStrLn "failed to add attributes: bottom scaffold"
         error_exit object_list 
      _       -> return ()

   v_bottom_grid_plus_attrib <- ( \(Just p) -> p ) <$> return ( maybe_bottom_scaffold )



   -------------------------------------------------------------------
   -- Setup info border vertex data
   -------------------------------------------------------------------

   -- define colour 
   let info_border_colour = [0.5, 0.042, 0.3]


   -- information border top
   let v_infotop_grid = ( createGrid2D ((-1.0) + ( unit_x * 4 ) )
                                 1.0
                                 unit_x
                                 unit_y
                                 57 
                                 2 ) :: [GLfloat]

   let maybe_top_info_border =
          addAttributesToGrid 2 57 2 (\_ _ -> info_border_colour) v_infotop_grid 

   case maybe_top_info_border of
      Nothing -> do
         putStrLn "failed to add attributes: information border top"
         error_exit object_list 
      _       -> return ()

   v_infotop_grid_plus_attrib <- ( \(Just p) -> p ) <$> return ( maybe_top_info_border )



   -- information border bottom 
   let v_infobottom_grid = ( createGrid2D ((-1.0) + ( unit_x * 4 ) )
                                 ( 1.0 - 2 * ( unit_y * 4 ) - (3 * unit_y) )
                                 unit_x
                                 unit_y
                                 57 
                                 2 ) :: [GLfloat]

   let maybe_bottom_info_border =
          addAttributesToGrid 2 57 2 (\_ _ -> info_border_colour) v_infobottom_grid

   case maybe_bottom_info_border of
      Nothing -> do
         putStrLn "failed to add attributes: information border bottom"
         error_exit object_list 
      _       -> return ()

   v_infobottom_grid_plus_attrib <- ( \(Just p) -> p ) <$> return ( maybe_bottom_info_border )



   -- information border left 
   let v_infoleft_grid = ( createGrid2D ((-1.0) + ( unit_x * 4 ) )
                                 ( 1.0 - unit_y )
                                 unit_x
                                 unit_y
                                 2 
                                 11 ) :: [GLfloat]

   let maybe_left_info_border =
          addAttributesToGrid 2 2 11 (\_ _ -> info_border_colour) v_infoleft_grid

   case maybe_left_info_border of
      Nothing -> do
         putStrLn "failed to add attributes: information border left"
         error_exit object_list 
      _       -> return ()

   v_infoleft_grid_plus_attrib <- ( \(Just p) -> p ) <$> return ( maybe_left_info_border )

 
   -- information border right 
   let v_inforight_grid = ( createGrid2D (1.0 - ( unit_x * 4 ) - unit_x )
                                 ( 1.0 - unit_y )
                                 unit_x
                                 unit_y
                                 2 
                                 11 ) :: [GLfloat]

   let maybe_right_info_border =
          addAttributesToGrid 2 2 11 (\_ _ -> info_border_colour) v_inforight_grid

   case maybe_right_info_border of
      Nothing -> do
         putStrLn "failed to add attributes: information border right"
         error_exit object_list 
      _       -> return ()

   v_inforight_grid_plus_attrib <- ( \(Just p) -> p ) <$> return ( maybe_right_info_border )


   --let vertices = [       
   --      -- positions            -- colours    
   --       (-0.75), 0.25,     1.0, 0.0, 1.0, -- Top 1    : 0
   --       (-0.25), 0.25,     1.0, 0.0, 0.0, -- Top 2    : 1
   --         0.25,  0.25,     0.0, 1.0, 0.0, -- Top 3    : 2
   --         0.75,  0.25,     0.0, 1.0, 1.0, -- Top 4    : 3
   --       (-0.75), (-0.25),  0.0, 0.0, 1.0, -- Bottom 1 : 4
   --       (-0.25), (-0.25),  1.0, 1.0, 1.0, -- Bottom 2 : 5
   --         0.25,  (-0.25),  0.0, 0.0, 0.0, -- Bottom 3 : 6
   --         0.75,  (-0.25),  1.0, 0.0, 1.0  -- Bottom 4 : 7
   --      ] :: [GLfloat] 



   -- combine scaffolding and information border vertices arrays

   let v_border = v_topleft_grid_plus_attrib ++
                    v_topright_grid_plus_attrib ++
                    v_bottomleft_grid_plus_attrib ++
                    v_bottomright_grid_plus_attrib ++
                    v_bottom_grid_plus_attrib ++
                    v_infotop_grid_plus_attrib ++
                    v_infobottom_grid_plus_attrib ++
                    v_infoleft_grid_plus_attrib ++
                    v_inforight_grid_plus_attrib


   let v_border_size = fromIntegral $ sizeOf ( 0.0 :: GLfloat ) * ( length v_border )
   v_border_ptr <- newArray v_border
   object_list <- return ( ( AllocFloatArr v_border_ptr ):object_list )


   -- switch to borders vertex array object
   vao_borders <- peekElemOff vao_ptr 0
   glBindVertexArray vao_borders

   -- select border vertex buffer object
   vbo_border <- peekElemOff vbo_ptr 0

   -- write border vertices to the vertex buffer object
   glBindBuffer GL_ARRAY_BUFFER vbo_border
   glBufferData GL_ARRAY_BUFFER v_border_size v_border_ptr GL_STATIC_DRAW

   let stride_position = fromIntegral $ 2 * ( sizeOf (0.0 :: GLfloat) ) :: GLsizei
   let stride_colours  = fromIntegral $ 3 * ( sizeOf (0.0 :: GLfloat) ) :: GLsizei
   let stride_vertices = stride_position + stride_colours
   let colour_offset_ptr = plusPtr nullPtr ( fromIntegral stride_position )

   -- enable vertex attribute 0 ( border positions )
   glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE stride_vertices nullPtr
   glEnableVertexAttribArray 0
   glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE stride_vertices colour_offset_ptr 
   glEnableVertexAttribArray 1

   -- enable vertex attribute 1 ( colour )
   --let colour_offset = plusPtr nullPtr ( 2 * (sizeOf (0.0 :: GLfloat) ) )
   --glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE stride colour_offset
   --glEnableVertexAttribArray 1

   --let indices = [
   --       0, 4, 5,  -- triangle 1
   --       0, 1, 5,  -- triangle 2 
   --       1, 5, 6,  -- triangle 3
   --       1, 2, 6,  -- triangle 4
   --       2, 6, 7,  -- triangle 5
   --       2, 3, 7   -- triangle 6
   --      ] :: [GLuint]


   -- add elements indices for scaffolding

   let i_topleft_grid = ( createIndicesArray2D 2 4 ) :: [GLuint]
   let i_topright_grid = ( createIndicesArray2D 2 4 ) :: [GLuint]
   let i_bottomleft_grid = ( createIndicesArray2D 3 10 ) :: [GLuint]
   let i_bottomright_grid = ( createIndicesArray2D 3 10 ) :: [GLuint]
   let i_bottom_grid = ( createIndicesArray2D 13 2 ) :: [GLuint]

   -- add information border elements indices
   let i_infotop_grid = ( createIndicesArray2D 57 2 ) :: [GLuint]
   let i_infobottom_grid = ( createIndicesArray2D 57 2 ) :: [GLuint]
   let i_infoleft_grid = ( createIndicesArray2D 2 11 ) :: [GLuint]
   let i_inforight_grid = ( createIndicesArray2D 2 11 ) :: [GLuint]

   -- combine element index arrays for borders

   let maybe_i_border = combineIndicesArrays2D [ i_topleft_grid,
                                                 i_topright_grid,
                                                 i_bottomleft_grid,
                                                 i_bottomright_grid,
                                                 i_bottom_grid,
                                                 i_infotop_grid,
                                                 i_infobottom_grid,
                                                 i_infoleft_grid,
                                                 i_inforight_grid ]
                                               [ v_topleft_grid_plus_attrib,
                                                 v_topright_grid_plus_attrib,
                                                 v_bottomleft_grid_plus_attrib,
                                                 v_bottomright_grid_plus_attrib,
                                                 v_bottom_grid_plus_attrib,
                                                 v_infotop_grid_plus_attrib,
                                                 v_infobottom_grid_plus_attrib,
                                                 v_infoleft_grid_plus_attrib,
                                                 v_inforight_grid_plus_attrib ] 5
                                               


   case maybe_i_border of
      Nothing -> do
         putStrLn "failed to combine indices arrays"
         error_exit object_list 
      _       -> return ()


   i_border <- (\ (Just x) -> x) <$> return maybe_i_border

   let i_border_size = fromIntegral $ sizeOf ( 0 :: GLuint ) * ( length i_border )
   i_border_ptr <- newArray i_border
   object_list <- return ( ( AllocUintArr i_border_ptr ):object_list )


   --putStrLn $ show vertices 
   --putStrLn $ show indices 

   ebo_border <- peekElemOff ebo_ptr 0

   -- write border indices to element buffer object
   glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo_border
   glBufferData GL_ELEMENT_ARRAY_BUFFER i_border_size i_border_ptr GL_STATIC_DRAW

   -- unbind border Vertex Array Object
   glBindVertexArray 0



   --------------------------------------------------
   -- Load data to draw snake icon
   --------------------------------------------------

   -- Compile snake icon shader program 
   maybe_shader_prog <- create_shader_program ( Just icon_vertex_shader_path )
                                                Nothing
                                              ( Just icon_fragment_shader_path )
   case maybe_shader_prog of
      Nothing -> do
         putStrLn "Failed to create shader program"
         error_exit object_list 
      _       -> return ()

   icon_shader_prog <- ( \(Just p) -> p ) <$> return ( maybe_shader_prog )
   object_list <- return ( ( AllocProgram icon_shader_prog ):object_list )

   -- Use the snake icon shader program
   glUseProgram icon_shader_prog


   -- set snake icon texture to fragment shader
   withCString "snakeTexture" $ \uniformNamePtr -> do
     uniformLoc <- glGetUniformLocation icon_shader_prog uniformNamePtr
     glUniform1i uniformLoc 0 

   glActiveTexture GL_TEXTURE0
   glBindTexture GL_TEXTURE_2D snake_texture


   -- switch to snake icon vertex array object
   vao_icon <- peekElemOff vao_ptr 1
   glBindVertexArray vao_icon

   -- draw snake icon area
   let icon_x = (-1.0) + ( unit_x * 12 ) 
   let icon_y = 1.0 - ( unit_y * 3.5 )
   let v_icon_grid = ( createGrid2D icon_x
                                    icon_y 
                                    ( unit_x * 4 )
                                    ( unit_y * 4 )
                                    2 
                                    2 ) :: [GLfloat]

   let icon_fxn = ( \x y -> (fromIntegral (x `mod` 2) :: Float):
                            (fromIntegral ((y `mod` 2) `xor` 1) :: Float):
                            [] ) -- arranges texture coordinate data correctly
   let maybe_icon = 
          addAttributesToGrid 2 2 2 icon_fxn v_icon_grid 

   case maybe_icon of
      Nothing -> do
         putStrLn "failed to add attributes: snake icon"
         error_exit object_list 
      _       -> return ()

   v_icon_grid_plus_attrib <- ( \(Just p) -> p ) <$> return ( maybe_icon )

   --putStrLn "icon grid"
   --printGrid v_icon_grid 2
   --putStrLn "icon grid with attributes"
   --printGrid v_icon_grid_plus_attrib 4 

   let v_icon_size = fromIntegral $ sizeOf ( 0.0 :: GLfloat ) *
                                    ( length v_icon_grid_plus_attrib )
   v_icon_ptr <- newArray v_icon_grid_plus_attrib
   object_list <- return ( ( AllocFloatArr v_icon_ptr ):object_list )

   -- select snake icon vertex buffer object
   vbo_icon <- peekElemOff vbo_ptr 1

   -- write snake icon vertices to the vertex buffer object
   glBindBuffer GL_ARRAY_BUFFER vbo_icon
   glBufferData GL_ARRAY_BUFFER v_icon_size v_icon_ptr GL_STATIC_DRAW

   let stride_position = fromIntegral $ 2 * ( sizeOf (0.0 :: GLfloat) ) :: GLsizei
   let stride_texture  = fromIntegral $ 2 * ( sizeOf (0.0 :: GLfloat) ) :: GLsizei
   let stride_vertices = stride_position + stride_texture
   let texture_offset_ptr = plusPtr nullPtr ( fromIntegral stride_position )

   -- enable vertex attribute 0 ( snake icon positions )
   glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE stride_vertices nullPtr
   glEnableVertexAttribArray 0
   glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE stride_vertices texture_offset_ptr 
   glEnableVertexAttribArray 1


   let i_icon_grid = ( createIndicesArray2D 2 2 ) :: [GLuint]

   let i_icon_size = fromIntegral $ sizeOf ( 0 :: GLuint ) * ( length i_icon_grid )
   i_icon_ptr <- newArray i_icon_grid
   object_list <- return ( ( AllocUintArr i_icon_ptr ):object_list )

   ebo_icon <- peekElemOff ebo_ptr 1

   -- write snake icon indices to element buffer object
   glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo_icon
   glBufferData GL_ELEMENT_ARRAY_BUFFER i_icon_size i_icon_ptr GL_STATIC_DRAW

   -- unbind snake icon Vertex Array Object
   glBindVertexArray 0




   -------------------------------------------------------------------
   -- Setup text 
   -------------------------------------------------------------------


   -- Compile text shader program 
   maybe_shader_prog <- create_shader_program ( Just text_vertex_shader_path )
                                                Nothing
                                              ( Just text_fragment_shader_path )
   case maybe_shader_prog of
      Nothing -> do
         putStrLn "Failed to create shader program"
         error_exit object_list 
      _       -> return ()

   text_shader_prog <- ( \(Just p) -> p ) <$> return ( maybe_shader_prog )
   object_list <- return ( ( AllocProgram icon_shader_prog ):object_list )

   -- Use the text shader program
   glUseProgram text_shader_prog

   -- create text vertices points
   let text_pixel_width = unit_x / 3
   let text_pixel_height = unit_y / 3

   let x_x = icon_x + (4 * unit_x) + (0.5 * unit_x)
   let x_y = icon_y - (4 * unit_y) + (text_pixel_height * 6)
   -- set starting vales to question marks as this will draw the error character
   -- from the text library. The error character creates information for every
   -- possible character pixel. That way, when we allocate memory for for the
   -- text, with newArray(), we should have enough for any combination of character.
   let x_points = makeStringPoints "X?"
                                   x_x
                                   x_y
                                   text_pixel_width
                                   text_pixel_height
                                   ( \_ -> [1.0, 1.0, 1.0, 1.0] )


   let score_text_x = x_x + (15 * unit_x)
   let score_text_y = x_y
   let score_text_points = makeStringPoints "SCORE ????"
                                            score_text_x
                                            score_text_y
                                            text_pixel_width
                                            text_pixel_height
                                            ( \_ -> [1.0, 1.0, 1.0, 1.0] )

   let v_text = x_points ++ score_text_points

   let v_text_size = fromIntegral $ sizeOf ( 0.0 :: GLfloat ) * ( length v_text )
   v_text_ptr <- newArray v_text
   object_list <- return ( ( AllocFloatArr v_text_ptr ):object_list )

   putStrLn "x"
   printGrid x_points 6


   -- switch to text vertex array object
   vao_text <- peekElemOff vao_ptr 2
   glBindVertexArray vao_text

   -- select text vertex buffer object
   vbo_text <- peekElemOff vbo_ptr 2

   -- write text vertices to the vertex buffer object
   glBindBuffer GL_ARRAY_BUFFER vbo_text
   glBufferData GL_ARRAY_BUFFER v_text_size v_text_ptr GL_STATIC_DRAW

   let text_stride_position = fromIntegral $ 2 * ( sizeOf (0.0 :: GLfloat) ) :: GLsizei
   let text_stride_colours  = fromIntegral $ 4 * ( sizeOf (0.0 :: GLfloat) ) :: GLsizei
   let text_stride_vertices = text_stride_position + text_stride_colours
   let text_colour_offset_ptr = plusPtr nullPtr ( fromIntegral text_stride_position )

   let num_text_vertices = fromIntegral $ ( length v_text ) `div` (2 + 4) :: GLsizei

   putStrLn $ "num vertices: " ++ ( show (fromIntegral num_text_vertices )) 

   -- enable vertex attributes for text vertices and colour
   glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE text_stride_vertices nullPtr
   glEnableVertexAttribArray 0
   glVertexAttribPointer 1 4 GL_FLOAT GL_FALSE text_stride_vertices text_colour_offset_ptr 
   glEnableVertexAttribArray 1


   -- unbind text Vertex Array Object
   glBindVertexArray 0



   let draw_list = []
   -- add background to draw list
   let i_len_border = ( fromIntegral (length i_border ) :: GLsizei )
   let bg_draw_info = DrawInfoBG vao_borders
                                 i_len_border
                                 bg_shader_prog
                                 Nothing
                                 Nothing
                                 Nothing
                                 True
   
   draw_list <- return $ ( Background bg_draw_info ):draw_list

   -- add icon to draw list
   let i_len_icon = ( fromIntegral (length i_icon_grid ) :: GLsizei )
   let icon_draw_info = DrawInfoBG vao_icon
                                   i_len_icon
                                   icon_shader_prog
                                   (Just snake_texture)
                                   Nothing
                                   Nothing
                                   False
   
   draw_list <- return $ ( Icon icon_draw_info ):draw_list


   -- add text to draw list
   let text_draw_info = DrawInfoTXT vao_text
                                   num_text_vertices
                                   text_shader_prog
                                   Nothing
                                   Nothing
                                   Nothing
                                   False
                                   v_text_ptr
                                   vbo_text
                                   x_x
                                   x_y 
   
   draw_list <- return $ ( Text text_draw_info ):draw_list





   -- Set Key Callback
   key_ref <- newIORef GLFW.Key'Unknown
   GLFW.setKeyCallback window ( Just $ key_callback key_ref )


   --let i_len_scaffold =( fromIntegral (length i_border ) :: GLsizei )
   --let i_len_info_border = ( fromIntegral (length i_info_border ) :: GLsizei )

   let game_state = GameState 0 2

   GLFW.setTime 0.0
   init_time <- ( maybe 0.0 id ) <$> GLFW.getTime

   game_loop window init_time draw_list game_state 

   

   cleanup_objects object_list
   GLFW.terminate
   putStrLn "Game Terminated!"
