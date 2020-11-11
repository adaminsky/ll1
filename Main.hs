import Text.ParserCombinators.Parsec
import ParseGrammar
import FirstFollow

readHelper :: IO [String]
readHelper = do
  x <- getLine
  if x == ""
    then return [""]
    else do xs <- readHelper
            return ((x ++ "\n"):xs)

main = do
  x <- readHelper
  putStrLn $ concat x
  case parse parseGrammar "" $ concat x of
    Left err -> putStrLn "Fail"
    Right g -> putStrLn $ show g
