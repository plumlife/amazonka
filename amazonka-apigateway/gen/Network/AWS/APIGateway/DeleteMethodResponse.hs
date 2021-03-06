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
-- Module      : Network.AWS.APIGateway.DeleteMethodResponse
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing MethodResponse resource.
--
-- /See:/ <http://docs.aws.amazon.com/apigateway/api-reference/resource/DeleteMethodResponse.html AWS API Reference> for DeleteMethodResponse.
module Network.AWS.APIGateway.DeleteMethodResponse
    (
    -- * Creating a Request
      deleteMethodResponse
    , DeleteMethodResponse
    -- * Request Lenses
    , dmRestAPIId
    , dmResourceId
    , dmHttpMethod
    , dmStatusCode

    -- * Destructuring the Response
    , deleteMethodResponseResponse
    , DeleteMethodResponseResponse
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | A request to delete an existing MethodResponse resource.
--
-- /See:/ 'deleteMethodResponse' smart constructor.
data DeleteMethodResponse = DeleteMethodResponse'
    { _dmRestAPIId  :: !Text
    , _dmResourceId :: !Text
    , _dmHttpMethod :: !Text
    , _dmStatusCode :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteMethodResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmRestAPIId'
--
-- * 'dmResourceId'
--
-- * 'dmHttpMethod'
--
-- * 'dmStatusCode'
deleteMethodResponse
    :: Text -- ^ 'dmRestAPIId'
    -> Text -- ^ 'dmResourceId'
    -> Text -- ^ 'dmHttpMethod'
    -> Text -- ^ 'dmStatusCode'
    -> DeleteMethodResponse
deleteMethodResponse pRestAPIId_ pResourceId_ pHttpMethod_ pStatusCode_ =
    DeleteMethodResponse'
    { _dmRestAPIId = pRestAPIId_
    , _dmResourceId = pResourceId_
    , _dmHttpMethod = pHttpMethod_
    , _dmStatusCode = pStatusCode_
    }

-- | The RestApi identifier for the MethodResponse resource.
dmRestAPIId :: Lens' DeleteMethodResponse Text
dmRestAPIId = lens _dmRestAPIId (\ s a -> s{_dmRestAPIId = a});

-- | The Resource identifier for the MethodResponse resource.
dmResourceId :: Lens' DeleteMethodResponse Text
dmResourceId = lens _dmResourceId (\ s a -> s{_dmResourceId = a});

-- | The HTTP verb identifier for the parent Method resource.
dmHttpMethod :: Lens' DeleteMethodResponse Text
dmHttpMethod = lens _dmHttpMethod (\ s a -> s{_dmHttpMethod = a});

-- | The status code identifier for the MethodResponse resource.
dmStatusCode :: Lens' DeleteMethodResponse Text
dmStatusCode = lens _dmStatusCode (\ s a -> s{_dmStatusCode = a});

instance AWSRequest DeleteMethodResponse where
        type Rs DeleteMethodResponse =
             DeleteMethodResponseResponse
        request = delete aPIGateway
        response = receiveNull DeleteMethodResponseResponse'

instance ToHeaders DeleteMethodResponse where
        toHeaders = const mempty

instance ToPath DeleteMethodResponse where
        toPath DeleteMethodResponse'{..}
          = mconcat
              ["/restapis/", toBS _dmRestAPIId, "/resources/",
               toBS _dmResourceId, "/methods/", toBS _dmHttpMethod,
               "/responses/", toBS _dmStatusCode]

instance ToQuery DeleteMethodResponse where
        toQuery = const mempty

-- | /See:/ 'deleteMethodResponseResponse' smart constructor.
data DeleteMethodResponseResponse =
    DeleteMethodResponseResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteMethodResponseResponse' with the minimum fields required to make a request.
--
deleteMethodResponseResponse
    :: DeleteMethodResponseResponse
deleteMethodResponseResponse = DeleteMethodResponseResponse'
