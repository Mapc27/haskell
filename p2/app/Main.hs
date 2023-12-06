module Main where
import Control.Applicative ((<$>), (<*>))
import System.Environment
import Text.Parsec
import Text.Parsec.String
import GHC.Real (reduce)


data CSV = CSV {header :: [String], rows :: [[String]]}
  deriving (Show)


getCell :: Char -> Parser String
getCell sep =
  do
    char '"'
    content <- many (noneOf ['"'])
    char '"'
    return content
    <|> many (noneOf [sep, '\n'])


getRow :: Char -> Parser [String]
getRow sep = getCell sep `sepBy` char sep <* newline


getRows :: Char -> Parser [[String]]
getRows sep = many (getRow sep)


getRowsWithHeader :: [String] -> Char -> Parser [[String]]
getRowsWithHeader header sep = do
  rows <- many (getRow sep)
  let wrongRows = filter (not . (\(i, row) -> length row == length header)) (zip [2..] rows)
  let headerLog row = " number of headers " ++ show (length row) ++ "/" ++ show (length header)
  if null wrongRows then
    return rows
  else do 
    let wrongRowsLog = foldl (\acc (i, row@(id : _)) -> acc ++ "\n" ++ id ++ "\t" ++ show i ++ headerLog row) ""
    fail $ "Wrong number of cells\n" ++ "Rows \n" ++  wrongRowsLog wrongRows


parseCSV :: Char -> Bool -> Parser CSV
parseCSV sep hasHeader = do
  header <- getRow sep
  rows <- 
    if hasHeader then
      getRowsWithHeader header sep
    else
      getRows sep
  return $ CSV header rows


mainProcess :: FilePath -> Char -> Bool -> IO (Either ParseError CSV)
mainProcess filePath sep hasHeader = do
  csvData <- readFile filePath
  return $ parse (parseCSV sep hasHeader) filePath csvData


main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath, sep, hasHeader] -> do
      result <- mainProcess filePath (head sep) (read hasHeader)
      case result of
        Left err -> putStrLn ("Error: " ++ show err)
        Right csv -> print csv


-- cabal run exes -- ./airbnb.csv , True
