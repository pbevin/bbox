import System.Environment
import Control.Monad
import Text.Regex (splitRegex, mkRegex)
import Data.List
import qualified Data.Text as T

cat :: [FilePath] -> IO String
cat [] = getContents
cat files = (sequence $ map readFile files) >>= return . concat

processLines :: ([String] -> [String]) -> [FilePath] -> IO String
processLines f files = (cat files) >>= return . unlines . f . lines

tac :: [FilePath] -> IO String
tac = processLines reverse

nl :: [FilePath] -> IO String
nl = processLines addNumber
  where addNumber lines = zipWith paste [1..] lines
        paste lineNo line = (width 6 $ show lineNo) ++ "\t" ++ line
        width w txt = (replicate (w-length txt) ' ') ++ txt

strip :: String -> String
strip = T.unpack . T.strip . T.pack

paragraphs :: String -> [String]
paragraphs = splitRegex (mkRegex "\n\n+")

unparagraphs :: [String] -> String
unparagraphs = intercalate "\n" . map (addNewline . strip)
  where addNewline para = para ++ "\n"

fmt :: [FilePath] -> IO String
fmt files = (cat files) >>= return . unparagraphs . map (unlines . reverse . foldl (addWord 76) [] . words) . paragraphs
  where addWord n [] word = [ word ]
        addWord n (line:lines) word
          | length line + 1 + length word < n = (line ++ " " ++ word) : lines
          | otherwise = word : line : lines

headCommand :: [FilePath] -> IO String
headCommand = processLines (take 10)

commands :: [(String, [FilePath] -> IO String)]
commands = [ ("cat", cat),
             ("tac", tac),
             ("nl", nl),
             ("fmt", fmt),
             ("head", headCommand) ]

main :: IO ()
main = do
  args <- getArgs
  case lookup (head args) commands of
    Just cmd -> (cmd $ tail args) >>= putStr
    Nothing -> putStrLn $ "No such command " ++ (head args)
