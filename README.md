# CSG-Haskell

In CSG-Haskell we can construct primitives and apply CSG operations on them (Union, Intersection, Difference).

Primitives aways receive translation, rotation and scale vectors respectively.

Here's one example of using CSG-Haskell

    Difference (
        Intersection (
            Cube (0 0 0) (0 0 0) (1 1 1)
            Sphere (0 0 0) (0 0 0) (1 1 1)
        )
        Union (
            Cylinder (0 0 0) (0 0 0) (1 1 1)
            Union (
                Cylinder (0 0 0) (0 0 0) (1 1 1)
                Cylinder (0 0 0) (0 0 0) (1 1 1)
            )
        )
    )

This code would produce the following tree:

![Example Image](http://jamie-wong.com/images/16-07-11/csg.png)