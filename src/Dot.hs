module Dot where
import Data.Set (Set)

class Dot a where
    dotNodes :: a -> Set String
    dotArrows :: a -> Set (String, String, String)
    dotName :: a -> String
    dotName _ = "graph"
    dotpreamble :: a -> String
    dotpreamble _ = ""

genDot :: Dot a => a -> String
genDot x = "digraph "++dotName x++" {\n"++dotpreamble x++nodes++arrows++"}"
  where
    nodes = foldMap (\y -> "    "++y++";\n") (dotNodes x)
    arrows = foldMap (\(y1,y2,y3) -> "    " ++y1++" -> "++y3++" [label = "++y2++"];\n") (dotArrows x)

showNoQuotes :: Show a => a -> [Char]
showNoQuotes y = filter (not . (=='\"')) $ show y

quotedString :: String -> String
quotedString x = "\""++x++"\""
