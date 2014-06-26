import System.Environment
import Control.Monad


cat :: [FilePath] -> IO String
cat [] = getContents
cat files = (sequence $ map readFile files) >>= return . concat

tac :: [FilePath] -> IO String
tac files = (cat files) >>= return . (unlines . reverse . lines)


main :: IO ()
main = do
  args <- getArgs
  case head args of
    "cat" -> (cat $ tail args) >>= putStr
    "tac" -> (tac $ tail args) >>= putStr
