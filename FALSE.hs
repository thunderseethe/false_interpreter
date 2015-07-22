import Util
import Exec
import Parser
import System.Environment

main = do
	args <- getArgs
	prog <- readFile $ head args
	(stack, reg) <- exec (parse prog) [] emptyReg -- init a-z registers to 0
	putStrLn $ show stack