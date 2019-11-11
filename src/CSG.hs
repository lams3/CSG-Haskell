module CSG where

type Vector = (Float, Float, Float)
type Position = Vector
type Rotation = Vector
type Scale = Vector
type Prop = (Position, Rotation, Scale)

data Primitive = Sphere Prop| Cube Prop | Cylinder Prop deriving Show

data Solid = Base Primitive | Union Solid Solid | Intersection Solid Solid | Difference Solid Solid deriving Show