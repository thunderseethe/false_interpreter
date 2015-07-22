module Util where
{- Handles common functionality between Exec and Parser -}

import Data.Char


data Operator = Dup | Drop | Swap | Rot | Pick | Plus | Minus | Multiply | Division | Negate | And | Or | Not | Greater | Equal | Comment | Execute | If | While | Store | Fetch deriving (Show)
data StdIO = ReadChar | WriteChar | WriteDec | Str String | Flush deriving (Show)
data Value = Op Operator | Num Double | Ch Char | Io StdIO | Fn [Value] | Var Int deriving (Show)

-- Util Functions
emptyReg = take 26 $ repeat (Num 0)

-- Converters
valueToBool :: Value -> Bool
valueToBool (Num 0) = False
valueToBool (Num n) = True
valueToBool _ = False

charToVar :: Char -> Int
charToVar c = (ord c) - (ord 'a')

charToNum :: Char -> Double
charToNum = fromIntegral . ord

numToChar :: Double -> Char
numToChar = chr . round

boolToNum :: Bool -> Double
boolToNum True = 1
boolToNum False = 0

numToBool :: Double -> Bool
numToBool 0 = False
numToBool n = True