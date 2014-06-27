import System.Environment
import Control.Monad

cat :: [FilePath] -> IO String
cat [] = getContents
cat files = (sequence $ map readFile files) >>= return . concat

processLines :: ([String] -> [String]) -> [FilePath] -> IO String
processLines f files = (cat files) >>= return . (unlines . f . lines)

tac :: [FilePath] -> IO String
tac = processLines reverse

nl :: [FilePath] -> IO String
nl = processLines $ addNumber
  where addNumber lines = zipWith paste [1..] lines
        paste lineNo line = (width 6 $ show lineNo) ++ "\t" ++ line
        width w txt = (replicate (w-length txt) ' ') ++ txt

main :: IO ()
main = do
  args <- getArgs
  case head args of
    "cat" -> (cat $ tail args) >>= putStr
    "tac" -> (tac $ tail args) >>= putStr
    "nl"  -> (nl  $ tail args) >>= putStr
