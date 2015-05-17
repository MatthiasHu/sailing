{-# LANGUAGE TemplateHaskell #-}

module Simulation
  ( Direction
  , degrees
  , Boat(..)
  , position, speed, heading
  , Sim(..)
  , boat, wind
  , sim0
  , Input(..)
  , input0
  , left, right
  , physicsTick
  )
  where

import Linear.V2
import Control.Lens
import Control.Lens.TH


type Direction = Float -- in rad

degrees :: Iso' Direction Float
degrees = iso (\rad -> 180*rad/pi)
              (\deg -> pi*deg/180)


data Boat = Boat
  { _position :: V2 Float
  , _speed    :: Float
  , _heading  :: Direction
  }
  deriving (Eq, Ord, Show)
makeLenses ''Boat

boat0 :: Boat
boat0 = Boat
  { _position = V2 0 0
  , _speed    = 0.001
  , _heading  = 2.2
  }

forwardMovement :: Getter Boat (V2 Float)
forwardMovement = to $ \b -> fmap (* view speed b) (angle $ view heading b)

move :: Boat -> Boat
move b = over position (+ view forwardMovement b) b


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
  { _left  :: Bool
  , _right :: Bool
  }
  deriving (Eq, Ord, Show)
makeLenses ''Input

input0 :: Input
input0 = Input
  { _left  = False
  , _right = False
  }


physicsTick :: Sim -> Sim
physicsTick = over boat move
