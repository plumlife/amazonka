{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Network.AWS.Response
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Response where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.ByteString.Lazy         as LBS
import           Data.Monoid
import           Data.Text                    (Text)
import           Network.AWS.Data.Body
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Log
import           Network.AWS.Data.XML
import           Network.AWS.Types
import           Network.HTTP.Client          hiding (Request, Response)
import           Network.HTTP.Types
import           Text.XML                     (Node)

import           Prelude

-- receiveNull :: MonadResource m
--             => Rs a
--             -> Logger
--             -> Service s
--             -> Request a
--             -> ClientResponse
--             -> m (Response a)
-- receiveNull rs _ = receive $ \_ _ x ->
--     liftResourceT (x $$+- return (Right rs))

-- receiveEmpty :: MonadResource m
--              => (Int -> ResponseHeaders -> () -> Either String (Rs a))
--              -> Logger
--              -> Service s
--              -> Request a
--              -> ClientResponse
--              -> m (Response a)
-- receiveEmpty f _ = receive $ \s h x ->
--     liftResourceT (x $$+- return (f s h ()))

-- receiveXMLWrapper :: MonadResource m
--                   => Text
--                   -> (Int -> ResponseHeaders -> [Node] -> Either String (Rs a))
--                   -> Logger
--                   -> Service s
--                   -> Request a
--                   -> ClientResponse
--                   -> m (Response a)
-- receiveXMLWrapper n f = receiveXML (\s h x -> x .@ n >>= f s h)

-- receiveXML :: MonadResource m
--            => (Int -> ResponseHeaders -> [Node] -> Either String (Rs a))
--            -> Logger
--            -> Service s
--            -> Request a
--            -> ClientResponse
--            -> m (Response a)
-- receiveXML = deserialise decodeXML

-- receiveJSON :: MonadResource m
--             => (Int -> ResponseHeaders -> Object -> Either String (Rs a))
--             -> Logger
--             -> Service s
--             -> Request a
--             -> ClientResponse
--             -> m (Response a)
-- receiveJSON = deserialise eitherDecode'

-- receiveBody :: MonadResource m
--             => (Int -> ResponseHeaders -> RsBody -> Either String (Rs a))
--             -> Logger
--             -> Service s
--             -> Request a
--             -> ClientResponse
--             -> m (Response a)
-- receiveBody f _ = receive $ \s h x -> return (f s h (RsBody x))

-- deserialise :: MonadResource m
--             => (LazyByteString -> Either String b)
--             -> (Int -> ResponseHeaders -> b -> Either String (Rs a))
--             -> Logger
--             -> Service s
--             -> Request a
--             -> ClientResponse
--             -> m (Response a)
-- deserialise g f l = receive $ \s h x -> do
--     lbs <- consumeLBS x
--     liftIO . l Debug . build $ "[Raw Response Body] {\n" <> lbs <> "\n}"
--     return $! g lbs >>= f s h

receive :: (Int -> ResponseHeaders -> BodyReader -> IO (Either String (Rs a)))
        -> Service s
        -> Request a
        -> ClientResponse
        -> IO (Response a)
receive f Service{..} _ rs
    | not (_svcStatus s) = consumeLBS x >>= serviceErr
    | otherwise          = do
        p <- f (fromEnum s) h x
        either serializeErr
               (return . (s,))
               p
  where
    s = responseStatus  rs
    h = responseHeaders rs
    x = responseBody    rs

    serviceErr :: MonadThrow m => LazyByteString -> m a
    serviceErr = throwM . _svcError _svcAbbrev s h

    serializeErr :: MonadThrow m => String -> m a
    serializeErr e = throwM (SerializeError (SerializeError' _svcAbbrev s e))

consumeLBS :: BodyReader -> IO LazyByteString
consumeLBS = liftM LBS.fromChunks . brConsume
