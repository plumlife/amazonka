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

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Conduit
import qualified Data.Conduit.Binary          as Conduit
import           Data.Monoid
import           Data.Text                    (Text)
import           Network.AWS.Data.Body
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Log
import           Network.AWS.Data.XML
import           Network.AWS.Error
import           Network.AWS.Types
import           Network.HTTP.Client          hiding (Request, Response)
import           Network.HTTP.Types
import           Text.XML                     (Node)

import           Prelude

receiveNull :: MonadResource m
            => Rs a
            -> Logger
            -> Service s
            -> Request a
            -> ClientResponse
            -> m (Response a)
receiveNull rs _ = receive $ \s h x -> do
    liftResourceT $ x $$+- return ()
    return . Right $ Response s (getRequestId h) rs

receiveEmpty :: MonadResource m
             => (Int -> ResponseHeaders -> () -> Either String (Rs a))
             -> Logger
             -> Service s
             -> Request a
             -> ClientResponse
             -> m (Response a)
receiveEmpty f _ = receive $ \s h x -> do
    liftResourceT $ x $$+- return ()
    return $ Response s (getRequestId h) <$> f s h ()

receiveXMLWrapper :: MonadResource m
                  => Text
                  -> (Int -> ResponseHeaders -> [Node] -> Either String (Rs a))
                  -> Logger
                  -> Service s
                  -> Request a
                  -> ClientResponse
                  -> m (Response a)
receiveXMLWrapper n f = receiveXML (\s h x -> x .@ n >>= f s h)

receiveXML :: MonadResource m
           => (Int -> ResponseHeaders -> [Node] -> Either String (Rs a))
           -> Logger
           -> Service s
           -> Request a
           -> ClientResponse
           -> m (Response a)
receiveXML = deserialise decodeXML getRequestIdXML

receiveJSON :: MonadResource m
            => (Int -> ResponseHeaders -> Object -> Either String (Rs a))
            -> Logger
            -> Service s
            -> Request a
            -> ClientResponse
            -> m (Response a)
receiveJSON = deserialise eitherDecode' (const getRequestId)

receiveBody :: MonadResource m
            => (Int -> ResponseHeaders -> RsBody -> Either String (Rs a))
            -> Logger
            -> Service s
            -> Request a
            -> ClientResponse
            -> m (Response a)
receiveBody f _ = receive $ \s h x ->
    return $ Response s (getRequestId h) <$> f s h (RsBody x)

deserialise :: MonadResource m
            => (LazyByteString -> Either String b)
            -> (b   -> ResponseHeaders -> Maybe RequestId)
            -> (Int -> ResponseHeaders -> b -> Either String (Rs a))
            -> Logger
            -> Service s
            -> Request a
            -> ClientResponse
            -> m (Response a)
deserialise g rid f l = receive $ \s h x -> do
    lbs <- sinkLBS x
    liftIO . l Debug . build $ "[Raw Response Body] {\n" <> lbs <> "\n}"
    return $ do
        d <- g lbs
        Response s (rid d h) <$> f s h d

receive :: MonadResource m
        => (Int -> ResponseHeaders -> ResponseBody -> m (Either String (Response a)))
        -> Service s
        -> Request a
        -> ClientResponse
        -> m (Response a)
receive f Service{..} _ rs
    | not (_svcStatus s) = sinkLBS x >>= serviceErr
    | otherwise          = f (fromEnum s) h x >>= either serializeErr return
  where
    s = responseStatus  rs
    h = responseHeaders rs
    x = responseBody    rs

    serviceErr :: MonadThrow m => LazyByteString -> m a
    serviceErr = throwM . _svcError _svcAbbrev s h

    serializeErr :: MonadThrow m => String -> m a
    serializeErr e = throwM (SerializeError (SerializeError' _svcAbbrev s e))

sinkLBS :: MonadResource m => ResponseBody -> m LazyByteString
sinkLBS bdy = liftResourceT (bdy $$+- Conduit.sinkLbs)

getRequestIdXML :: [Node] -> ResponseHeaders -> Maybe RequestId
getRequestIdXML x h = getRequestId h <|> query <|> ec2
  where
    query = may (firstElement "RequestId" x)

    -- <AddPermissionResponse>
    --     <ResponseMetadata>
    --         <RequestId>
    --             9a285199-c8d6-47c2-bdb2-314cb47d599d
    --         </RequestId>
    --     </ResponseMetadata>
    -- </AddPermissionResponse>

    ec2 = may (firstElement "requestId" x)

    -- <DescribeKeyPairsResponse xmlns="http://ec2.amazonaws.com/doc/2015-04-15/">
    --   <requestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</requestId>
    --   ...

    may (Left  _) = Nothing
    may (Right x) = either (const Nothing) Just (parseXML x)
