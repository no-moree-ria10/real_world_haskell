{-# OPTIONS_GHC -cpp #-}
-- file: ch09/BetterPredicate.hs
module BetterPredicate () where

import Control.Monad (filterM)
import System.Directory ( Permissions(..), getModificationTime, getPermissions) 
import Data.Time ( UTCTime(..) )
import System.FilePath( takeExtension)
import Control.Exception (bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
--以前書いた関数
import RecursiveContents (getRecursiveContents)
--Exception Tipe Signature用
import GHC.Exception

type Predicate = FilePath --ディレクトリエントリへのパス
                 -> Permissions
                 -> Maybe Integer --File size (not file -> Nothing)
                 -> UTCTime --ClockTime
                 -> Bool  --純粋なことに注意する

getFileSize :: FilePath -> IO ( Maybe Integer )
getFileSize = undefined
betterFind :: Predicate -> FilePath -> IO [FilePath]

betterFind p path = getRecursiveContents path >>= filterM check
  where check name = do
          perms <- getPermissions name
          size <- getFileSize name
          modified <- getModificationTime name
          return (p name perms size modified )
        
        
simpleFileSize :: FilePath -> IO Integer
simpleFileSize path = do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return size
  
saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path =  handle ( (\_ -> return Nothing ) :: SomeException-> IO ( (Maybe Integer)) )$ do  --例外の型が不明なこととNothingの型が不明なことから注釈が必要
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return (Just size)
  