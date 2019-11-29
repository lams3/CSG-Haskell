module CSG.Rendering (
    getShader
) where
    
import Data.Text (pack, unpack, replace)

import CSG
import CSG.Parser

getShader :: Solid -> IO String
getShader s = do
    contents <- readFile "shaders/fragmentShader.glsl"
    return $ replace' "@sceneSDF" sceneSDF contents
    where 
        sceneSDF = concat ["float sceneSDF(vec3 p) {", "p = rotateY(time) * p;", "return ", solidToString s, ";", "}"] 
        replace' s1 s2 = unpack . replace (pack s1) (pack s2) . pack

solidToString :: Solid -> String
solidToString (Base p t) = primitiveToString p t 
solidToString (Union s1 s2) = concat ["unionSDF(", solidToString s1, ", " , solidToString s2, ")"]
solidToString (Intersection s1 s2) = concat ["intersectSDF(", solidToString s1, ", " , solidToString s2, ")"]
solidToString (Difference s1 s2) = concat ["differenceSDF(", solidToString s1, ", " , solidToString s2, ")"]

primitiveToString :: Primitive -> Transform -> String
primitiveToString (Box s) (t, r) = concat ["boxSDF(p, ", vectorToString s, ", ", vectorToString t, ", ", vectorToString r,")"]
primitiveToString (Sphere rad) (t, r) = concat ["sphereSDF(p, ", show rad, ", ", vectorToString t, ", ", vectorToString r, ")"]
primitiveToString (Cylinder h rad) (t, r) = concat ["cylinderSDF(p, ", show h, ", ", show rad, ", ", vectorToString t, ", ", vectorToString r, ")"]

vectorToString :: Vector -> String
vectorToString (x, y, z) = concat ["vec3(", show x, ", ", show y, ", ", show z, ")"]