module CSG.Parser (
    parse
) where

import CSG
import Text.ParserCombinators.ReadP
import Data.Char
import Control.Applicative hiding (many)

readF :: String -> Float
readF = read

isWhiteSpace :: Char -> Bool
isWhiteSpace char = elem char "     \n"

whiteSpace :: ReadP ()
whiteSpace = do
    _ <- many (satisfy isWhiteSpace)
    return ()

whiteSpace1 :: ReadP ()
whiteSpace1 = do
    _ <- many1 (satisfy isWhiteSpace)
    return ()

digit :: ReadP Char
digit = satisfy isDigit

integer :: ReadP Float
integer = fmap readF (many1 digit)

float :: ReadP Float
float = do
    i <- integer
    _ <- string "."
    d <- integer
    return (i + (readF ("0." ++ (show $ round d))))

number :: ReadP Float
number = float <|> integer

vector :: ReadP Vector
vector = do
    _ <- string "("
    _ <- whiteSpace
    x <- number
    _ <- whiteSpace1
    y <- number
    _ <- whiteSpace1   
    z <- number
    _ <- whiteSpace    
    _ <- string ")"
    return (x, y, z)

cube :: ReadP (Prop -> Primitive)
cube = do 
    x <- string "Cube" <|> string "cube"
    case x of
        [] -> pfail
        _ -> return Cube

cylinder :: ReadP (Prop -> Primitive)
cylinder = do 
    x <- string "Cylinder" <|> string "cylinder"
    case x of
        [] -> pfail
        _ -> return Cylinder

sphere :: ReadP (Prop -> Primitive)
sphere = do 
    x <- string "Sphere" <|> string "sphere"
    case x of
        [] -> pfail
        _ -> return Sphere

primitiveType :: ReadP (Prop -> Primitive)
primitiveType = cube <|> cylinder <|> sphere 

primitive :: ReadP Primitive
primitive = do
    pType <- primitiveType
    _ <- whiteSpace
    p <- vector
    _ <- whiteSpace
    r <- vector
    _ <- whiteSpace
    s <- vector
    return (pType (p, r, s))

union :: ReadP (Solid -> Solid -> Solid)
union = do 
    x <- string "Union" <|> string "union"
    case x of
        [] -> pfail
        _ -> return Union

intersection :: ReadP (Solid -> Solid -> Solid)
intersection = do 
    x <- string "Intersection" <|> string "intersection"
    case x of
        [] -> pfail
        _ -> return Intersection

difference :: ReadP (Solid -> Solid -> Solid)
difference = do 
    x <- string "Difference" <|> string "difference"
    case x of
        [] -> pfail
        _ -> return Difference

operationType :: ReadP (Solid -> Solid -> Solid)
operationType = union <|> intersection <|> difference

operation :: ReadP Solid
operation = do
    oType <- operationType
    _ <- whiteSpace
    _ <- string "("
    _ <- whiteSpace
    s1 <- solid
    _ <- whiteSpace
    s2 <- solid
    _ <- whiteSpace
    _ <- string ")"
    return (oType s1 s2)

solid :: ReadP Solid
solid = ((fmap Base primitive) <|> operation)

parse :: String -> Maybe Solid
parse str =
    case readP_to_S solid str of
        [] -> Nothing
        (sol, _):_ -> Just sol