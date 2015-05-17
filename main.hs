
import Graphics.UI.GLUT
import Simulation as S
import Data.IORef
import RenderUtils
import Control.Lens
import Control.Concurrent (threadDelay)
import Linear.V2


main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  createWindow "Sailing"
  glSetup
  simRef <- newIORef S.sim0
  inputRef <- newIORef S.input0
  displayCallback $= (get simRef >>= display)
  idleCallback    $= Just (idle simRef)
  mainLoop

glSetup :: IO ()
glSetup = do
  windowSize     $= Size 512 512
  windowPosition $= Position 0 0
  clearColor     $= Color4 0 0 0.5 1
  depthFunc      $= Just Always

idle :: IORef S.Sim -> IdleCallback
idle simRef = do
  threadDelay 20000
  modifyIORef simRef S.physicsTick
  postRedisplay Nothing

display :: Sim -> DisplayCallback
display sim = do
  clear [ColorBuffer]
  -- render the boat
  loadIdentity
  translate' $ sim ^. S.boat . S.position
  rotate'    $ sim ^. S.boat . S.heading
  renderBoat
  -- render wind arrow
  loadIdentity
  translate' $ V2 (-0.8) 0.8
  scale' 0.15
  renderWindArrow $ sim ^. S.wind
  flush


renderBoat :: IO ()
renderBoat = do
  color' 0.8 0.6 0.3
  scale' 0.1
  renderPrimitive LineLoop
    $ mapM_ vertex' boatVertices

boatVertices :: [V2 Float]
boatVertices = map (\(x, y) -> V2 x y) $
 [( 1.0, 0.0)
 ,( 0.6, 0.3)
 ,( 0.0, 0.4)
 ,(-0.6, 0.3)
 ,(-0.6,-0.3)
 ,( 0.0,-0.4)
 ,( 0.6,-0.3)
 ]

renderWindArrow :: V2 Float -> IO ()
renderWindArrow wind = do
  color' 0.5 0.7 0.9
  renderPrimitive Lines $ mapM_ vertex'
    $ map (\(V2 x y) -> fmap (x*) wind + fmap (y*) (perp wind))
          windArrowVertices

windArrowVertices :: [V2 Float]
windArrowVertices = map (\(x, y) -> V2 x y) $
  [(-1.0, 0.0)
  ,( 1.0, 0.0)
  ,( 1.0, 0.0)
  ,( 0.5, 0.5)
  ,( 1.0, 0.0)
  ,( 0.5,-0.5)
  ]
