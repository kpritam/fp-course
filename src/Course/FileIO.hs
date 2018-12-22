{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

-- Given the file name, and file contents, print them.
printFile :: FilePath -> Chars -> IO ()
printFile fp c = do
  _ <- putStrLn ("============ " ++ fp)
  putStrLn c

-- Given a list of (file name and file contents), print each.
printFiles :: List (FilePath, Chars) -> IO ()
printFiles xs = void (sequence (uncurry printFile <$> xs))

-- Given a file name, return (file name and file contents).
getFile :: FilePath -> IO (FilePath, Chars)
getFile path = (\x -> (path, x)) <$> (readFile path)

-- Given a list of file names, return list of (file name and file contents).
getFiles :: List FilePath -> IO (List (FilePath, Chars))
getFiles paths = sequence(getFile <$>  paths)

-- Given a file name, read it and for each line in that file, read and print contents of each.
run :: FilePath -> IO ()
run fp = do
  content <- readFile fp
  files <- getFiles (lines content)
  printFiles files

main :: IO ()
main = do
  args <- getArgs
  case args of
    Nil -> putStrLn "No argument provided."
    fp :. _ -> run fp
