{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase, TupleSections #-}

module Common (exitWithError, createDirectoryRecursive, outdated) where

import System.IO 
import System.Exit
import System.Posix.Files
import System.Posix.Directory
import System.FilePath.Posix

import Control.Monad

import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Calendar

import Data.List


exitWithError :: String -> IO a
exitWithError msg = do
  hPutStrLn stderr msg
  exitWith $ ExitFailure 1

createDirectoryRecursive :: String -> IO ()
createDirectoryRecursive dir = forM_ (tail $ inits $ splitPath dir) $ \ps -> do
  let path = joinPath ps
  exists <- fileExist path
  when (not exists) $ createDirectory path accessModes

outdated :: String -> IO Bool
outdated path = fileExist path >>= \case
  True -> do
    modified <- posixSecondsToUTCTime . modificationTimeHiRes
      <$> getFileStatus path
    now <- getCurrentTime
    return $ addUTCTime maxAge modified < now
  False -> return True

maxAge :: NominalDiffTime
maxAge = fromIntegral 1800
