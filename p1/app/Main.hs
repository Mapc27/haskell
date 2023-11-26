import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import GHC.Desugar (toAnnotationWrapper)
import System.Directory hiding (getFileSize)
import System.Environment
import System.FilePath
import System.IO


data Options = Options
  { 
    depth :: Int
  , humanReadable :: Bool
  , verbose :: Bool
  , rootPath :: FilePath
  }


type IOMonad a = ExceptT String (WriterT String (ReaderT Options (StateT Integer IO))) a


processDir :: FilePath -> Int -> IOMonad Integer
processDir path currentDepth = do
  options <- ask
  contents <- liftIO $ listDirectory path
  sizes <- mapM (\name -> du (path </> name) (currentDepth + 1)) contents
  let allSize = sum sizes
  let depthExceeded = currentDepth > depth options
  let humanReadableSize  = if humanReadable options then gethumanReadableSize allSize else show allSize
  when (not depthExceeded || (depth options == (-1))) $ tell $ humanReadableSize ++ "\t" ++ path ++ "\n"
  
  when (verbose options) $ liftIO $ putStrLn("process directory: " ++ path)
  return allSize


getFileSize :: FilePath -> IOMonad Integer
getFileSize filePath = do
  options <- ask
  size <- liftIO $ withFile filePath ReadMode hFileSize
  let humanReadableSize = if humanReadable options then gethumanReadableSize size else show size
  when (depth options == -1) $ tell $ humanReadableSize ++ "\t" ++ filePath ++ "\n"
  
  when (verbose options) $ liftIO $ putStrLn("get size of file: " ++ filePath)
  return size



du :: FilePath -> Int -> IOMonad Integer
du path currentDepth = do
  isDir <- liftIO (doesDirectoryExist path)
  if isDir then
    processDir path currentDepth
  else
    getFileSize path


gethumanReadableSize :: Integer -> String
gethumanReadableSize size =
  if size < 1024 then
    show size ++ "B"
  else 
    if size < 1024 ^ 2 then
      show (div size 1024) ++ "KB"
    else
      if size < 1024 ^ 3 then
        show (div size (1024 ^ 2)) ++ "MB"
      else 
        show (div size (1024 ^ 3)) ++ "GB"


run :: Options -> IO (Either String Integer, String)
run options = do
  (result, log) <- runStateT (runReaderT (runWriterT (runExceptT (du rootPathValue 0))) options) 0
  return result
  where
    rootPathValue = rootPath options


parseArgs :: [String] -> Options
parseArgs args = foldl setOptions options args
  where
    options = Options (-1) False False "/"

    setOptions options "-d" = options {depth = read (head args)}
    setOptions options "-s" = options {depth = 0}
    setOptions options "-h" = options {humanReadable = True}
    setOptions options "-v" = options {verbose = True}
    setOptions options dir = options {rootPath = dir}


main :: IO ()
main = do
  args <- getArgs
  let options = parseArgs args
  (result, log) <- run options
  case result of
    Left exception -> putStrLn("Exception: " ++ exception)
    Right size -> putStrLn(log)
