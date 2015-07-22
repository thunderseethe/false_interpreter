module Exec where

import Util

exec :: [Value] -> [Value] -> [Value] -> IO ([Value], [Value])
exec [] stack reg = return (stack, reg)

-- Handle Arithmetic
exec ((Op Plus):prog) ((Num a):(Num b):stack) reg = exec prog ((Num (b+a)):stack) reg
exec ((Op Minus):prog) ((Num a):(Num b):stack) reg = exec prog ((Num (b-a)):stack) reg
exec ((Op Multiply):prog) ((Num a):(Num b):stack) reg = exec prog ((Num (b*a)):stack) reg
exec ((Op Division):prog) ((Num a):(Num b):stack) reg = exec prog ((Num (b/a)):stack) reg
exec ((Op Negate):prog) ((Num a):stack) reg = exec prog ((Num (negate a)):stack) reg

-- Handle Comparison
exec ((Op Equal):prog) ((Num a):(Num b):stack) reg = exec prog ((Num (boolToNum (b == a))):stack) reg
exec ((Op Greater):prog) ((Num a):(Num b):stack) reg = exec prog ((Num (boolToNum (b > a))):stack) reg

-- Handle Boolean Algebra
exec ((Op And):prog) ((Num a):(Num b):stack) reg = exec prog ((Num (b `nAnd` a)):stack) reg
exec ((Op Or):prog) ((Num a):(Num b):stack) reg = exec prog ((Num (b `nOr` a)):stack) reg
exec ((Op Not):prog) ((Num a):stack) reg = exec prog ((Num (nNot a)):stack) reg

-- Handle Stack Operation
exec ((Op Dup):prog) (a:stack) reg = exec prog (a:a:stack) reg
exec ((Op Drop):prog) (a:stack) reg = exec prog stack reg
exec ((Op Swap):prog) (a:b:stack) reg = exec prog (b:a:stack) reg
exec ((Op Rot):prog) (a:b:c:stack) reg = exec prog (c:a:b:stack) reg
exec ((Op Pick):prog) ((Num a):stack) reg = exec prog ((pick a stack):stack) reg

-- Handle Control Flow
exec ((Op Execute):prog) ((Fn lambda):stack) reg =  do
	(newStack, newReg) <- exec lambda stack reg
	exec prog newStack newReg

exec ((Op If):prog) (fn:(Num a):stack) reg = 
	if (numToBool a)
	then exec ((Op Execute):prog) (fn:stack) reg
	else exec prog stack reg

exec true@((Op While):prog) ((Fn body):(Fn cond):stack) reg = do
	(condStack, condReg) <- exec cond stack reg
	if (while condStack)
	then do
		(newStack, newReg) <- (exec body stack condReg)
		exec true ((Fn body):(Fn cond):newStack) newReg
	else exec prog stack condReg

-- Handle Standard Input/Output
exec ((Io ReadChar):prog) stack reg = do
	char <- getChar
	exec prog ((Num (charToNum char)):stack) reg
exec ((Io WriteChar):prog) ((Num a):stack) reg = do
	putChar $ numToChar a
	exec prog stack reg
exec ((Io WriteDec):prog) ((Num a):stack) reg = do
	putStr $ show a
	exec prog stack reg
exec ((Io (Str str)):prog) stack reg = do
	putStr str
	exec prog stack reg

-- Handle Variables Operations
exec ((Op Store):prog) ((Var key):val:stack) reg = exec prog stack (put reg key val) 
exec ((Op Fetch):prog) ((Var key):stack) reg = exec prog ((get reg key):stack) reg

-- Handle Values
exec ((Op op):prog) stack reg = error ("Invalid operator parameters: " ++ (show op)) --(exec prog stack reg)
exec ((Ch c):prog) stack reg = exec prog ((Num (charToNum c)):stack) reg
exec (a:prog) stack reg = exec prog (a:stack) reg

-- FALSE operators
pick :: Double -> [Value] -> Value
pick n stack = head $ drop (round n) stack

while = valueToBool . head

put' :: [Value] -> [Value] -> Value -> Int -> [Value]
put' left (del:right) val 0 = left ++ [val] ++ right
put' left (mov:right) val key = put' (left++[mov]) right val (key-1)

put :: [Value] -> Int -> Value -> [Value]
put reg key val = put' [] reg val key 

get :: [Value] -> Int -> Value
get (val:reg) 0 = val
get (val:reg) key = get reg (key - 1)

-- Boolean Num Operators
nAnd :: Double -> Double -> Double
nAnd 0.0 0.0 = 0.0
nAnd n 0.0 = 0.0
nAnd 0.0 n = 0.0
nAnd n m = 1.0

nNot :: Double -> Double
nNot 0.0 = 1.0
nNot n = 0.0

nOr :: Double -> Double -> Double
nOr 0.0 0.0 = 0.0
nOr n 0.0 = 1.0
nOr 0.0 n = 1.0
nOr n m = 1.0