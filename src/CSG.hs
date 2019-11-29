module CSG where

type Vector = (Float, Float, Float)

type Position = Vector
type Rotation = Vector

type Transform = (Position, Rotation)

data Primitive = 
    Sphere { radius::Float } | 
    Box { side::Vector } | 
    Cylinder { height::Float, radius::Float } 
    deriving Show

data Solid = 
    Base Primitive Transform | 
    Union Solid Solid | 
    Intersection Solid Solid | 
    Difference Solid Solid 
    deriving Show