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
-- Module      : Network.AWS.StorageGateway.RemoveTagsFromResource
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation removes one or more tags from the specified resource.
--
-- /See:/ <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_RemoveTagsFromResource.html AWS API Reference> for RemoveTagsFromResource.
module Network.AWS.StorageGateway.RemoveTagsFromResource
    (
    -- * Creating a Request
      removeTagsFromResource
    , RemoveTagsFromResource
    -- * Request Lenses
    , rtfrTagKeys
    , rtfrResourceARN

    -- * Destructuring the Response
    , removeTagsFromResourceResponse
    , RemoveTagsFromResourceResponse
    -- * Response Lenses
    , rtfrrsResourceARN
    , rtfrrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types
import           Network.AWS.StorageGateway.Types.Product

-- | RemoveTagsFromResourceInput
--
-- /See:/ 'removeTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
    { _rtfrTagKeys     :: !(Maybe [Text])
    , _rtfrResourceARN :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RemoveTagsFromResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtfrTagKeys'
--
-- * 'rtfrResourceARN'
removeTagsFromResource
    :: RemoveTagsFromResource
removeTagsFromResource =
    RemoveTagsFromResource'
    { _rtfrTagKeys = Nothing
    , _rtfrResourceARN = Nothing
    }

-- | The keys of the tags you want to remove from the specified resource. A
-- tag is composed of a key\/value pair.
rtfrTagKeys :: Lens' RemoveTagsFromResource [Text]
rtfrTagKeys = lens _rtfrTagKeys (\ s a -> s{_rtfrTagKeys = a}) . _Default . _Coerce;

-- | The Amazon Resource Name (ARN) of the resource you want to remove the
-- tags from.
rtfrResourceARN :: Lens' RemoveTagsFromResource (Maybe Text)
rtfrResourceARN = lens _rtfrResourceARN (\ s a -> s{_rtfrResourceARN = a});

instance AWSRequest RemoveTagsFromResource where
        type Rs RemoveTagsFromResource =
             RemoveTagsFromResourceResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 RemoveTagsFromResourceResponse' <$>
                   (x .?> "ResourceARN") <*> (pure (fromEnum s)))

instance ToHeaders RemoveTagsFromResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.RemoveTagsFromResource" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RemoveTagsFromResource where
        toJSON RemoveTagsFromResource'{..}
          = object
              (catMaybes
                 [("TagKeys" .=) <$> _rtfrTagKeys,
                  ("ResourceARN" .=) <$> _rtfrResourceARN])

instance ToPath RemoveTagsFromResource where
        toPath = const "/"

instance ToQuery RemoveTagsFromResource where
        toQuery = const mempty

-- | RemoveTagsFromResourceOutput
--
-- /See:/ 'removeTagsFromResourceResponse' smart constructor.
data RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse'
    { _rtfrrsResourceARN    :: !(Maybe Text)
    , _rtfrrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RemoveTagsFromResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtfrrsResourceARN'
--
-- * 'rtfrrsResponseStatus'
removeTagsFromResourceResponse
    :: Int -- ^ 'rtfrrsResponseStatus'
    -> RemoveTagsFromResourceResponse
removeTagsFromResourceResponse pResponseStatus_ =
    RemoveTagsFromResourceResponse'
    { _rtfrrsResourceARN = Nothing
    , _rtfrrsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the resource that the tags were
-- removed from.
rtfrrsResourceARN :: Lens' RemoveTagsFromResourceResponse (Maybe Text)
rtfrrsResourceARN = lens _rtfrrsResourceARN (\ s a -> s{_rtfrrsResourceARN = a});

-- | The response status code.
rtfrrsResponseStatus :: Lens' RemoveTagsFromResourceResponse Int
rtfrrsResponseStatus = lens _rtfrrsResponseStatus (\ s a -> s{_rtfrrsResponseStatus = a});
