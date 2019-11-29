# CSG-Haskell

In CSG-Haskell we can construct primitives and apply CSG operations on them (Union, Intersection, Difference).

Primitives aways receive translation, rotation and scale vectors respectively.

Here's one example of using CSG-Haskell

    Difference (
        Intersection (
            Box (1 1 1) T( (0 0 0) (0 0 0) )
            Sphere 1 T( (0 0 0) (0 0 0) )
        )
        Union (
            Cylinder 1 0.5 T( (0 0 0) (0 0 0) )
            Union (
                Cylinder 1 0.5 T( (0 0 0) (90 0 0) )
                Cylinder 1 0.5 T( (0 0 0) (0 0 90) )
            )
        )
    )

This code would produce the following tree:

![Example Image](http://jamie-wong.com/images/16-07-11/csg.png)