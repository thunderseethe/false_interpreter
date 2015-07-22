module Parser where

import Data.Char
import Util

parse :: [Char] -> [Value]
parse [] = []
-- Parse Arithmetic
parse ('+':prog) = (Op Plus):(parse prog)
parse ('-':prog) = (Op Minus):(parse prog)
parse ('*':prog) = (Op Multiply):(parse prog)
parse ('/':prog) = (Op Division):(parse prog)
parse ('_':prog) = (Op Negate):(parse prog)

-- Parse Comparison
parse ('=':prog) = (Op Equal):(parse prog)
parse ('>':prog) = (Op Greater):(parse prog)

-- Parse Boolean Algebra
parse ('&':prog) = (Op And):(parse prog)
parse ('|':prog) = (Op Or):(parse prog)
parse ('~':prog) = (Op Not):(parse prog)

-- Parse Stack Op
parse ('$':prog) = (Op Dup):(parse prog)
parse ('%':prog) = (Op Drop):(parse prog)
parse ('\\':prog) = (Op Swap):(parse prog)
parse ('@':prog) = (Op Rot):(parse prog)
parse ('o':prog) = (Op Pick):(parse prog)

-- Parse Control Flow
parse ('!':prog) = (Op Execute):(parse prog)
parse ('?':prog) = (Op If):(parse prog)
parse ('#':prog) = (Op While):(parse prog)
parse ('{':prog) = (parse (afterComment prog))
parse ('[':prog) = (Fn (parse $ parseLambda 0 prog)):(parse $ afterLambda 0 prog)

-- Parse Standard Input/Output
parse ('^':prog) = (Io ReadChar):(parse prog)
parse (',':prog) = (Io WriteChar):(parse prog)
parse ('.':prog) = (Io WriteDec):(parse prog)
parse ('"':prog) = (Io (Str (parseString prog))):(parse $ afterString prog)

-- Parse Variables
parse (c:prog) | isAlpha c = (Var (charToVar c)):(parse prog)
parse (':':prog) = (Op Store):(parse prog)
parse (';':prog) = (Op Fetch):(parse prog)

-- Parse Literals
parse ('\'':c:prog) = (Ch c):(parse prog)
parse full@(i:prog) | isDigit i = (Num (read $ parseNum full)):(parse (afterNum prog))

-- Skip other shit
parse (c:prog) = (parse prog)


-- Parse manipulators
-- Parse Strings
parseString :: [Char] -> [Char]
parseString ('"':prog) = []
-- Handle special characters explicitly
parseString ('\\':'a':prog) = (chr 7):(parseString prog)
parseString ('\\':'b':prog) = (chr 8):(parseString prog)
parseString ('\\':'f':prog) = (chr 12):(parseString prog)
parseString ('\\':'n':prog) = (chr 10):(parseString prog)
parseString ('\\':'r':prog) = (chr 13):(parseString prog)
parseString ('\\':'t':prog) = (chr 9):(parseString prog)
parseString ('\\':'"':prog) = (chr 34):(parseString prog)
parseString (c:prog) = c:(parseString prog)

afterString :: [Char] -> [Char]
afterString ('"':prog) = prog
afterString (c:prog) = afterString prog


-- Parse Lambdas
parseLambda :: Int -> [Char] -> [Char]
parseLambda n [] = error "Unclosed lambda"
parseLambda 0 (']':prog) = []
parseLambda n (']':prog) = ']':(parseLambda (n-1) prog)
parseLambda n ('[':prog) = '[':(parseLambda (n+1) prog)
parseLambda n (c:prog) = c:(parseLambda n prog)

afterLambda :: Int -> [Char] -> [Char]
afterLambda n [] = error "Unclosed lambda"
afterLambda 0 (']':prog) = prog
afterLambda n (']':prog) = afterLambda (n-1) prog
afterLambda n ('[':prog) = afterLambda (n+1) prog
afterLambda n (c:prog) = afterLambda n prog

-- Parse Numbers
parseNum :: [Char] -> [Char]
parseNum [] = []
parseNum ('.':i:prog) | isDigit i = '.':i:(parseNum prog)
parseNum (i:prog) | isDigit i = i:(parseNum prog)
parseNum (c:prog) = []

afterNum :: [Char] -> [Char]
afterNum [] = []
afterNum (i:prog) | isDigit i = afterNum prog
afterNum full@(c:prog) = full


-- Ignore Comments
afterComment :: [Char] -> [Char]
afterComment [] = error "Unclosed Comment"
afterComment ('}':prog) = prog
afterComment (_:prog) = afterComment prog