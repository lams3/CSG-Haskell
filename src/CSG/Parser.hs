module CSG.Parser (
    parse
) where

import CSG
import Text.ParserCombinators.ReadP
import Data.Char
import Control.Applicative hiding (many)
import Control.Monad

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
number = do
    s <- string "+" <|> string "-" <|> string ""
    n <- float <|> integer
    case s of
        "+" -> return n
        "" -> return n
        "-" -> return (-n)

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

transform :: ReadP Transform
transform = do
    _ <- string "T"
    _ <- whiteSpace
    _ <- string "("
    _ <- whiteSpace
    t <- vector
    _ <- whiteSpace1
    r <- vector
    _ <- whiteSpace
    _ <- string ")"
    return (t, r)

box :: ReadP Primitive
box = do 
    x <- string "Box" <|> string "box"
    _ <- whiteSpace1
    side <- vector
    case x of
        [] -> pfail
        _ -> return Box { side = side }

cylinder :: ReadP Primitive
cylinder = do 
    x <- string "Cylinder" <|> string "cylinder"
    _ <- whiteSpace1
    height <- number
    _ <- whiteSpace1
    radius <- number
    case x of
        [] -> pfail
        _ -> return Cylinder { height = height, radius = radius}

sphere :: ReadP Primitive
sphere = do 
    x <- string "Sphere" <|> string "sphere"
    _ <- whiteSpace1
    radius <- number
    case x of
        [] -> pfail
        _ -> return Sphere { radius = radius }

primitive :: ReadP Primitive
primitive = box <|> cylinder <|> sphere 

base :: ReadP Solid
base = do
    primitive <- primitive
    _ <- whiteSpace1
    transform <- transform
    return (Base primitive transform)

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
solid = base <|> operation

parse :: String -> Maybe Solid
parse str =
    case readP_to_S solid str of
        [] -> Nothing
        (sol, _):_ -> Just sol