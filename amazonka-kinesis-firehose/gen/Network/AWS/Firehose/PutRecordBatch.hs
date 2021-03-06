{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.PutRecordBatch
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Writes multiple data records into a delivery stream in a single call,
-- which can achieve higher throughput per producer than when writing
-- single records. To write single data records into a delivery stream, use
-- PutRecord. Applications using these operations are referred to as
-- producers.
--
-- Each PutRecordBatch request supports up to 500 records. Each record in
-- the request can be as large as 1,000 KB (before 64-bit encoding), up to
-- a limit of 4 MB for the entire request. By default, each delivery stream
-- can take in up to 2,000 transactions per second, 5,000 records per
-- second, or 5 MB per second. Note that if you use PutRecord and
-- PutRecordBatch, the limits are an aggregate across these two operations
-- for each delivery stream. For more information about limits and how to
-- request an increase, see
-- <http://docs.aws.amazon.com/firehose/latest/dev/limits.html Amazon Kinesis Firehose Limits>.
--
-- You must specify the name of the delivery stream and the data record
-- when using PutRecord. The data record consists of a data blob that can
-- be up to 1,000 KB in size, and any kind of data, for example, a segment
-- from a log file, geographic location data, web site clickstream data,
-- and so on.
--
-- Amazon Kinesis Firehose buffers records before delivering them to the
-- destination. To disambiguate the data blobs at the destination, a common
-- solution is to use delimiters in the data, such as a newline ('\\n') or
-- some other character unique within the data. This allows the consumer
-- application(s) to parse individual data items when reading the data from
-- the destination.
--
-- The PutRecordBatch response includes a count of any failed records,
-- 'FailedPutCount', and an array of responses, 'RequestResponses'. The
-- 'FailedPutCount' value is a count of records that failed. Each entry in
-- the 'RequestResponses' array gives additional information of the
-- processed record. Each entry in 'RequestResponses' directly correlates
-- with a record in the request array using the same ordering, from the top
-- to the bottom of the request and response. 'RequestResponses' always
-- includes the same number of records as the request array.
-- 'RequestResponses' both successfully and unsuccessfully processed
-- records. Amazon Kinesis Firehose attempts to process all records in each
-- PutRecordBatch request. A single record failure does not stop the
-- processing of subsequent records.
--
-- A successfully processed record includes a 'RecordId' value, which is a
-- unique value identified for the record. An unsuccessfully processed
-- record includes 'ErrorCode' and 'ErrorMessage' values. 'ErrorCode'
-- reflects the type of error and is one of the following values:
-- 'ServiceUnavailable' or 'InternalFailure'. 'ErrorMessage' provides more
-- detailed information about the error.
--
-- If 'FailedPutCount' is greater than 0 (zero), retry the request. A retry
-- of the entire batch of records is possible; however, we strongly
-- recommend that you inspect the entire response and resend only those
-- records that failed processing. This minimizes duplicate records and
-- also reduces the total bytes sent (and corresponding charges).
--
-- If the PutRecordBatch operation throws a 'ServiceUnavailableException',
-- back off and retry. If the exception persists, it is possible that the
-- throughput limits have been exceeded for the delivery stream.
--
-- Data records sent to Amazon Kinesis Firehose are stored for 24 hours
-- from the time they are added to a delivery stream as it attempts to send
-- the records to the destination. If the destination is unreachable for
-- more than 24 hours, the data is no longer available.
--
-- /See:/ <http://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html AWS API Reference> for PutRecordBatch.
module Network.AWS.Firehose.PutRecordBatch
    (
    -- * Creating a Request
      putRecordBatch
    , PutRecordBatch
    -- * Request Lenses
    , prbDeliveryStreamName
    , prbRecords

    -- * Destructuring the Response
    , putRecordBatchResponse
    , PutRecordBatchResponse
    -- * Response Lenses
    , prbrsResponseStatus
    , prbrsFailedPutCount
    , prbrsRequestResponses
    ) where

import           Network.AWS.Firehose.Types
import           Network.AWS.Firehose.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for PutRecordBatch.
--
-- /See:/ 'putRecordBatch' smart constructor.
data PutRecordBatch = PutRecordBatch'
    { _prbDeliveryStreamName :: !Text
    , _prbRecords            :: !(List1 Record)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutRecordBatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prbDeliveryStreamName'
--
-- * 'prbRecords'
putRecordBatch
    :: Text -- ^ 'prbDeliveryStreamName'
    -> NonEmpty Record -- ^ 'prbRecords'
    -> PutRecordBatch
putRecordBatch pDeliveryStreamName_ pRecords_ =
    PutRecordBatch'
    { _prbDeliveryStreamName = pDeliveryStreamName_
    , _prbRecords = _List1 # pRecords_
    }

-- | The name of the delivery stream.
prbDeliveryStreamName :: Lens' PutRecordBatch Text
prbDeliveryStreamName = lens _prbDeliveryStreamName (\ s a -> s{_prbDeliveryStreamName = a});

-- | One or more records.
prbRecords :: Lens' PutRecordBatch (NonEmpty Record)
prbRecords = lens _prbRecords (\ s a -> s{_prbRecords = a}) . _List1;

instance AWSRequest PutRecordBatch where
        type Rs PutRecordBatch = PutRecordBatchResponse
        request = postJSON firehose
        response
          = receiveJSON
              (\ s h x ->
                 PutRecordBatchResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "FailedPutCount") <*>
                     (x .:> "RequestResponses"))

instance ToHeaders PutRecordBatch where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Firehose_20150804.PutRecordBatch" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutRecordBatch where
        toJSON PutRecordBatch'{..}
          = object
              (catMaybes
                 [Just
                    ("DeliveryStreamName" .= _prbDeliveryStreamName),
                  Just ("Records" .= _prbRecords)])

instance ToPath PutRecordBatch where
        toPath = const "/"

instance ToQuery PutRecordBatch where
        toQuery = const mempty

-- | Contains the output of PutRecordBatch.
--
-- /See:/ 'putRecordBatchResponse' smart constructor.
data PutRecordBatchResponse = PutRecordBatchResponse'
    { _prbrsResponseStatus   :: !Int
    , _prbrsFailedPutCount   :: !Nat
    , _prbrsRequestResponses :: !(List1 PutRecordBatchResponseEntry)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutRecordBatchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prbrsResponseStatus'
--
-- * 'prbrsFailedPutCount'
--
-- * 'prbrsRequestResponses'
putRecordBatchResponse
    :: Int -- ^ 'prbrsResponseStatus'
    -> Natural -- ^ 'prbrsFailedPutCount'
    -> NonEmpty PutRecordBatchResponseEntry -- ^ 'prbrsRequestResponses'
    -> PutRecordBatchResponse
putRecordBatchResponse pResponseStatus_ pFailedPutCount_ pRequestResponses_ =
    PutRecordBatchResponse'
    { _prbrsResponseStatus = pResponseStatus_
    , _prbrsFailedPutCount = _Nat # pFailedPutCount_
    , _prbrsRequestResponses = _List1 # pRequestResponses_
    }

-- | The response status code.
prbrsResponseStatus :: Lens' PutRecordBatchResponse Int
prbrsResponseStatus = lens _prbrsResponseStatus (\ s a -> s{_prbrsResponseStatus = a});

-- | The number of unsuccessfully written records.
prbrsFailedPutCount :: Lens' PutRecordBatchResponse Natural
prbrsFailedPutCount = lens _prbrsFailedPutCount (\ s a -> s{_prbrsFailedPutCount = a}) . _Nat;

-- | The results for the individual records. The index of each element
-- matches the same index in which records were sent.
prbrsRequestResponses :: Lens' PutRecordBatchResponse (NonEmpty PutRecordBatchResponseEntry)
prbrsRequestResponses = lens _prbrsRequestResponses (\ s a -> s{_prbrsRequestResponses = a}) . _List1;
