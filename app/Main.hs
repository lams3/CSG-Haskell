{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Data.Maybe
import Data.Time.Clock.System
import Foreign
import Foreign.C.String
import Foreign.C.Types
import System.Exit
import System.IO
import System.Environment (getArgs)

import Graphics.GL
import qualified Graphics.UI.GLFW as GLFW

import CSG
import CSG.Parser
import CSG.Rendering

data GLIDs =
    GLIDs {
        progId :: !GLuint,
        vertexArrayId :: !GLuint,
        vertexBufferId :: !GLuint
    }

withNewPtr :: Storable b => (Ptr b -> IO a) -> IO b
withNewPtr f = alloca (\p -> f p >> peek p)

initialize :: IO GLFW.Window
initialize = do
    ok <- GLFW.init
    unless ok $ do
        _ <- fail "Failed to initialize GLFW"
        exitFailure
    mapM_ GLFW.windowHint
        [
            GLFW.WindowHint'Samples (Just 4),
            GLFW.WindowHint'ContextVersionMajor 3,
            GLFW.WindowHint'ContextVersionMinor 3,
            GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
        ]
    win <- GLFW.createWindow 800 600 "CSG-Haskell" Nothing Nothing
    when (isNothing win) $ do
        _ <- fail "Failed to create OpenGL window"
        GLFW.terminate
        exitFailure
    let win' = fromJust win
    GLFW.makeContextCurrent win
    GLFW.setStickyKeysInputMode win' GLFW.StickyKeysInputMode'Enabled
    return win'

initializeGL :: String -> IO GLIDs
initializeGL path = do
    contents <- readFile path
    case parse contents of
        Just s -> do
            glClearColor 0 0 0 0
            vShader <- vertexShader
            fShader <- getShader s
            progId <- loadProgram vShader fShader
            vaId <- newVAO
            bufId <- fillNewBuffer vertexBufferData
            return $ GLIDs {
                progId = progId,
                vertexArrayId = vaId,
                vertexBufferId = bufId
            }
        Nothing -> return $ GLIDs {
            progId = 0,
            vertexArrayId  = 0,
            vertexBufferId = 0
        }

freeResources :: GLIDs -> IO ()
freeResources GLIDs{..} = do
    with vertexBufferId $ glDeleteBuffers 1
    with vertexArrayId $ glDeleteBuffers 1

newVAO :: IO GLuint
newVAO = do
    vaId <- withNewPtr (glGenVertexArrays 1)
    glBindVertexArray vaId
    return vaId

fillNewBuffer :: [GLfloat] -> IO GLuint
fillNewBuffer xs = do
    bufId <- withNewPtr (glGenBuffers 1)
    glBindBuffer GL_ARRAY_BUFFER bufId
    withArrayLen xs func
    return bufId
    where
        func len ptr =
            glBufferData
                GL_ARRAY_BUFFER
                (fromIntegral  (len * sizeOf (undefined :: GLfloat)))
                (ptr :: Ptr GLfloat)
                GL_STATIC_DRAW

bindBufferToAttrib :: GLuint -> GLuint -> IO ()
bindBufferToAttrib bufId attribLoc = do
    glEnableVertexAttribArray attribLoc
    glBindBuffer GL_ARRAY_BUFFER bufId
    glVertexAttribPointer
        attribLoc
        3
        GL_FLOAT
        (fromBool False)
        0
        nullPtr

loadProgram :: String -> String -> IO GLuint
loadProgram vertShader fragShader = do
    shaderIds <- mapM (uncurry loadShader)
        [
            (GL_VERTEX_SHADER, vertShader),
            (GL_FRAGMENT_SHADER, fragShader)
        ]
    progId <- glCreateProgram
    putStrLn "Linking program"
    mapM_ (glAttachShader progId) shaderIds
    glLinkProgram progId
    _ <- checkStatus
        GL_LINK_STATUS glGetProgramiv glGetProgramInfoLog progId
    mapM_ glDeleteShader shaderIds
    return progId

loadShader :: GLenum -> String -> IO GLuint
loadShader shaderTypeFlag code = do
    shaderId <- glCreateShader shaderTypeFlag
    withCString code $ \codePtr ->
        with codePtr $ \codePtrPtr ->
            glShaderSource shaderId 1 codePtrPtr nullPtr
    putStrLn "Compiling shader..."
    glCompileShader shaderId
    _ <- checkStatus
        GL_COMPILE_STATUS glGetShaderiv glGetShaderInfoLog shaderId
    return shaderId

checkStatus :: (Integral a1, Storable a1) =>
    GLenum ->
    (t -> GLenum -> Ptr a1 -> IO a) ->
    (t -> a1 -> Ptr a3 -> Ptr Foreign.C.Types.CChar -> IO a2) ->
    t ->
    IO Bool
checkStatus statusFlag glGetFn glInfoLogFn componentId = do
    let
        fetch info = withNewPtr (glGetFn componentId info)
    status <- fmap toBool $ fetch statusFlag
    logLength <- fetch GL_INFO_LOG_LENGTH
    when (logLength > 0) $
        allocaArray0 (fromIntegral logLength) $ \msgPtr -> do
        _ <- glInfoLogFn componentId logLength nullPtr msgPtr
        msg <- peekCString msgPtr
        (if status then putStrLn else fail) msg
    return status

vertexShader :: IO String
vertexShader = readFile "shaders/vertexShader.glsl"

vertexBufferData :: [GLfloat]
vertexBufferData =
    [
        -1, -1, 0,
        1, -1, 0,
        1, 1, 0,
        -1, 1, 0
    ]

main :: IO ()
main = do
    args <- getArgs
    let path = args !! 0
    win <- initialize
    glids <- initializeGL path
    inputLoop win glids path ""
    GLFW.terminate
    return ()

inputLoop :: GLFW.Window -> GLIDs -> String -> String -> IO ()
inputLoop win glids path oldText = do
    newText <- readFile path
    if (newText /= oldText) then do
        freeResources glids
        newGLIDs <- initializeGL path
        inputLoop win newGLIDs path newText
    else do
        draw glids
        GLFW.swapBuffers win
        GLFW.pollEvents
        keyState <- GLFW.getKey win GLFW.Key'Escape
        closeWindow <- GLFW.windowShouldClose win
        when (keyState /= GLFW.KeyState'Pressed && closeWindow == False) $
            inputLoop win glids path newText

draw :: GLIDs -> IO ()
draw GLIDs{..} = do
    glClear GL_COLOR_BUFFER_BIT
    glClear GL_DEPTH_BUFFER_BIT
    glUseProgram progId
    bindBufferToAttrib vertexBufferId 0
    glDrawArrays GL_TRIANGLE_FAN 0 4
    glDisableVertexAttribArray 0

-- main = do
--     contents <- readFile "testFiles/Test.csg"
--     case parse contents of
--         Nothing -> putStrLn "Couldn't parse Test.csg"
--         Just s -> print s
