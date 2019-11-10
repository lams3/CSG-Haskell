# CSG-Haskell

In CSG-Haskell we can construct primitives and apply CSG operations on them (Union, Intersection, Difference).

Primitives aways receive translation, rotation and scale vectors respectively.

Here's one example for using CSG-Haskell

    Difference (
        Intersection (
            Cube (x y z) (a b g) (sx sy sz)
            Sphere (x y z) (a b g) (sx sy sz)
        )
        Union (
            Cylinder (x y z) (a b g) (sx sy sz)
            Union (
                Cylinder (x y z) (a b g) (sx sy sz)
                Cylinder (x y z) (a b g) (sx sy sz)
            )
        )
    )

This code would produce the following tree:

![Example Image](http://jamie-wong.com/images/16-07-11/csg.png)