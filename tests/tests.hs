{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main where

import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Control.Monad.Trans.Resource (MonadResource, runResourceT)
import           Data.Default                 (def)
import           System.IO.Temp               (withSystemTempDirectory)

import           Database.RocksDB             (Compression (..), DB, compression,
                                               createIfMissing, defaultOptions, get, open,
                                               put)

import qualified Data.ByteString              as BS

import           Database.RocksDB.Iterator    (iterFirst, iterNext, iterValid,
                                                unsafeIterKey, unsafeIterValue,
                                                withIter)
import           Database.RocksDB.ReadOptions (ReadOptions, withReadOptions)

import           Test.Hspec                   (describe, hspec, it, shouldReturn)
import           Test.QuickCheck              (Arbitrary (..), UnicodeString (..),
                                               generate)

initializeDB :: MonadResource m => FilePath -> m DB
initializeDB path =
    open
        path
        defaultOptions
        {createIfMissing = True, compression = NoCompression}

main :: IO ()
main =  hspec $ do

  describe "Basic DB Functionality" $ do
    it "should put items into the database and retrieve them" $  do
      runResourceT $ withSystemTempDirectory "rocksdb" $ \path -> do
        db <- initializeDB path
        put db def "zzz" "zzz"
        get db mempty "zzz"
      `shouldReturn` (Just "zzz")

    it "should put items into a database whose filepath has unicode characters and\
       \ retrieve them" $  do
      runResourceT $ withSystemTempDirectory "rocksdb" $ \path -> do
        unicode <- getUnicodeString <$> liftIO (generate arbitrary)
        db <- initializeDB $ path ++ unicode
        put db def "zzz" "zzz"
        get db mempty "zzz"
      `shouldReturn` (Just "zzz")

    it "should read keys and values via unsafeIterKey and unsafeIterValue" $ do
      runResourceT $ withSystemTempDirectory "rocksdb" $ \path -> do
        db <- initializeDB path
        put db def "key1" "value1"
        put db def "key2" "value2"
        put db def "key3" "value3"
        -- The continuations use 'BS.copy' so the results are independent of the
        -- iterator-owned buffer, which is only valid until the iterator moves.
        liftIO $ withReadOptions (mempty :: ReadOptions) $ \opts_ptr ->
          withIter db opts_ptr $ \iter -> do
            iterFirst iter
            let go acc = do
                  valid <- iterValid iter
                  if not valid
                    then return (reverse acc)
                    else do
                      mk <- unsafeIterKey   iter BS.copy
                      mv <- unsafeIterValue iter BS.copy
                      iterNext iter
                      case (mk, mv) of
                        (Just k, Just v) -> go ((k, v) : acc)
                        _                -> go acc
            go []
      `shouldReturn`
        [ ("key1", "value1")
        , ("key2", "value2")
        , ("key3", "value3")
        ]

