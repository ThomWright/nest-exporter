{-# LANGUAGE NamedFieldPuns #-}

module Web.Nest.FileCache
  ( withFileCache
  ) where

import           Control.Exception    (tryJust)
import           Control.Monad        (guard)
import           Data.Aeson           (FromJSON, ToJSON, decode, encode)

import qualified Data.ByteString.Lazy as LBS
import           Data.Text            (Text, pack)
import           System.IO.Error      (isDoesNotExistError)

withFileCache ::
     ((FromJSON a), (ToJSON a))
  => FilePath
  -> IO (Either Text a)
  -> IO (Either Text a)
withFileCache filePath action = do
  cacheFileContents <- readJsonFile filePath
  case cacheFileContents of
    UnparsableJSON ->
      return
        (Left (("Unparsable JSON in cache file: " :: Text) <> pack filePath))
    FileNotFound -> do
      putStrLn ("No cache file found (" ++ filePath ++ "), attempting to fill")
      result <- action
      case result of
        Left e -> return (Left e)
        Right x
          -- cache result in file
         -> do
          LBS.writeFile filePath (encode x)
          putStrLn ("Filled cache file (" ++ filePath ++ ")")
          return (Right x)
    CachedValue x -> return (Right x)

readJsonFile :: (FromJSON a) => FilePath -> IO (CacheFileResult a)
readJsonFile filepath = do
  fileContents <- tryJust (guard . isDoesNotExistError) (LBS.readFile filepath)
  case fileContents of
    Right fileContents' ->
      case decode fileContents' of
        Just decoded -> return (CachedValue decoded)
        Nothing      -> return UnparsableJSON
    Left _ -> return FileNotFound

data CacheFileResult a
  = FileNotFound
  | UnparsableJSON
  | CachedValue a
  deriving (Eq, Show)
