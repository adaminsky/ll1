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

loop :: Grammar -> IO ()
loop g = do
  var <- getLine
  case parse nonterminal "" var of
    Left err -> putStrLn "Fail"
    Right nt -> do let firsts = show $ firstSet g nt
                       follows = show $ followSet g nt
                   putStrLn $ "First Set of " ++ show nt ++ " is: " ++ firsts
                   putStrLn $ "Follow Set of " ++ show nt ++ " is: " ++ follows
                   loop g

main = do
  x <- readHelper
  putStrLn "Queries:"
  case parse parseGrammar "" $ concat x of
    Left err -> putStrLn "Fail"
    Right g -> loop g
