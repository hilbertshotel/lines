module Main where

-- IMPORTS
----------------------------------------------------
import Msg
import System.Environment
import System.Directory
import Control.Concurrent


-- FUNCTIONS
----------------------------------------------------
countLines :: String -> IO Int
countLines file = readFile file >>= \content ->
  return $ length $ lines content


isFolderOrFile :: String -> IO (Bool, Bool)
isFolderOrFile path = doesDirectoryExist path >>= \dirExists ->
  doesFileExist path >>= \fileExists -> return (dirExists, fileExists)


extendPaths :: [String] -> String -> [String]
extendPaths [] _ = [] 
extendPaths (first:rest) dir = fullPath : extendPaths rest dir
  where fullPath = dir ++ "/" ++ first


checkTypes :: [String] -> Int -> IO Int
checkTypes [] result = return result
checkTypes (first:rest) result = isFolderOrFile first >>= \case
    
    (True, _) -> do
      listContents <- listDirectory first
      let list = extendPaths listContents first
      numOfLines <- checkTypes list 0
      checkTypes rest (numOfLines + result)

    (_, True) -> do
      numOfLines <- countLines first
      checkTypes rest (numOfLines + result)


validatePaths :: [String] -> [String] -> IO String
validatePaths [] all = checkTypes all 0 >>= return . show
validatePaths (first:rest) all = doesPathExist first >>= \case
  True -> validatePaths rest all
  False -> return $ Msg.errorNotExist first


-- MAIN
----------------------------------------------------
match :: [String] -> IO String
match [] = return Msg.info
match args = validatePaths args args

main :: IO ()
main = getArgs >>= match >>= putStrLn