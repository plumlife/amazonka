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
-- Module      : Network.AWS.APIGateway.CreateResource
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Resource resource.
--
-- /See:/ <http://docs.aws.amazon.com/apigateway/api-reference/resource/CreateResource.html AWS API Reference> for CreateResource.
module Network.AWS.APIGateway.CreateResource
    (
    -- * Creating a Request
      createResource
    , CreateResource
    -- * Request Lenses
    , crRestAPIId
    , crParentId
    , crPathPart

    -- * Destructuring the Response
    , resource
    , Resource
    -- * Response Lenses
    , rPathPart
    , rPath
    , rId
    , rResourceMethods
    , rParentId
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Requests Amazon API Gateway to create a Resource resource.
--
-- /See:/ 'createResource' smart constructor.
data CreateResource = CreateResource'
    { _crRestAPIId :: !Text
    , _crParentId  :: !Text
    , _crPathPart  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crRestAPIId'
--
-- * 'crParentId'
--
-- * 'crPathPart'
createResource
    :: Text -- ^ 'crRestAPIId'
    -> Text -- ^ 'crParentId'
    -> Text -- ^ 'crPathPart'
    -> CreateResource
createResource pRestAPIId_ pParentId_ pPathPart_ =
    CreateResource'
    { _crRestAPIId = pRestAPIId_
    , _crParentId = pParentId_
    , _crPathPart = pPathPart_
    }

-- | The identifier of the RestApi for the resource.
crRestAPIId :: Lens' CreateResource Text
crRestAPIId = lens _crRestAPIId (\ s a -> s{_crRestAPIId = a});

-- | The parent resource\'s identifier.
crParentId :: Lens' CreateResource Text
crParentId = lens _crParentId (\ s a -> s{_crParentId = a});

-- | The last path segment for this resource.
crPathPart :: Lens' CreateResource Text
crPathPart = lens _crPathPart (\ s a -> s{_crPathPart = a});

instance AWSRequest CreateResource where
        type Rs CreateResource = Resource
        request = postJSON aPIGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders CreateResource where
        toHeaders = const mempty

instance ToJSON CreateResource where
        toJSON CreateResource'{..}
          = object
              (catMaybes [Just ("pathPart" .= _crPathPart)])

instance ToPath CreateResource where
        toPath CreateResource'{..}
          = mconcat
              ["/restapis/", toBS _crRestAPIId, "/resources/",
               toBS _crParentId]

instance ToQuery CreateResource where
        toQuery = const mempty
