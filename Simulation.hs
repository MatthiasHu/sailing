{-# LANGUAGE TemplateHaskell #-}

module Simulation
  ( Direction
  , degrees
  , Sail(..)
  , mast, orientation
  , Boat(..)
  , position, speed, heading, sails
  , Sim(..)
  , boat, wind
  , sim0
  , Input(..)
  , keysDown
  , input0
  , physicsTick
  , inputApplication
  )
  where

import Linear.V2
import Linear.Metric
import Linear.Vector
import Control.Lens
import Control.Lens.TH
import Data.Set hiding (map)
import Graphics.UI.GLUT (Key(..))


type Direction = Float -- in rad

degrees :: Iso' Direction Float
degrees = iso (\rad -> 180*rad/pi)
              (\deg -> pi*deg/180)

data Sail = Sail
  { _mast        :: V2 Float
  , _orientation :: Direction
  , _keyLeft     :: Key
  , _keyRight    :: Key
  }
  deriving (Eq, Ord, Show)
makeLenses ''Sail

data Boat = Boat
  { _position :: V2 Float
  , _speed    :: Float
  , _heading  :: Direction
  , _sails    :: [Sail]
  }
  deriving (Eq, Ord, Show)
makeLenses ''Boat

boat0 :: Boat
boat0 = Boat
  { _position = V2 0 0
  , _speed    = 0.0
  , _heading  = 1.2
  , _sails    =
      [ Sail { _mast        = V2 0.03 0.0
             , _orientation = 0.0 
             , _keyLeft     = Char 'r'
             , _keyRight    = Char 'f'       }
      , Sail { _mast        = V2 (-0.03) 0.0
             , _orientation = 0.0 
             , _keyLeft     = Char 'e'
             , _keyRight    = Char 'd'       }
      ]
  }


data Sim = Sim
  { _boat :: Boat
  , _wind :: V2 Float
  }
  deriving (Eq, Ord, Show)
makeLenses ''Sim

sim0 :: Sim
sim0 = Sim
  { _boat = boat0
  , _wind = V2 1 0
  }


data Input = Input
  { _keysDown :: Set Key
  }
  deriving (Eq, Ord, Show)
makeLenses ''Input

input0 :: Input
input0 = Input empty


physicsTick :: Sim -> Sim
physicsTick sim = boat %~ movement $ sim
  where
    movement = rotation . leeway . forward . friction . speedup
    rotation = over heading (+1.5*totalTorque)
    leeway   = over position (+ 0.001*(perp $ angle oldHeading)
                                 ^*(totalSailForce ^. _y)      )
    forward  = over position (+ (angle oldHeading)
                                ^*oldSpeed        )
    friction = over speed $ \v -> if v > 0 then 0.9*v
                                           else 0.8*v
    speedup  = over speed (+ 0.005*(totalSailForce ^. _x))
    sailForces = map sailForce $ theboat ^. sails
    totalSailForce  = sum (map fst    sailForces)
    totalTorque     = sum (map torque sailForces)
    torque (f, q) = f `dot` (perp q)
    theboat = sim ^. boat
    oldHeading = theboat ^. heading
    oldSpeed   = theboat ^. speed
    sailForce s = ( relativeNormal ^* 0.05
                      ^* absoluteNormal `dot` (sim^.wind)
                  , s ^. mast                     )
      where relativeNormal = angle (s^.orientation)
            absoluteNormal = angle $ (s ^. orientation) + (theboat ^. heading)


inputApplication :: Input -> Sim -> Sim
inputApplication input sim = boat . sails %~ map (sailInput input) $ sim

sailInput :: Input -> Sail -> Sail
sailInput input sail = 
  case ( (sail^.keyLeft)  `member` (input^.keysDown)
       , (sail^.keyRight) `member` (input^.keysDown)) of
    (True , False) -> orientation %~ (+  0.1 ) $ sail
    (False, True ) -> orientation %~ (+(-0.1)) $ sail
    _              -> sail
