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
-- Module      : Network.AWS.APIGateway.GetStage
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Stage resource.
--
-- /See:/ <http://docs.aws.amazon.com/apigateway/api-reference/resource/GetStage.html AWS API Reference> for GetStage.
module Network.AWS.APIGateway.GetStage
    (
    -- * Creating a Request
      getStage
    , GetStage
    -- * Request Lenses
    , gssRestAPIId
    , gssStageName

    -- * Destructuring the Response
    , stage
    , Stage
    -- * Response Lenses
    , sDeploymentId
    , sVariables
    , sClientCertificateId
    , sCreatedDate
    , sCacheClusterStatus
    , sMethodSettings
    , sLastUpdatedDate
    , sCacheClusterSize
    , sCacheClusterEnabled
    , sStageName
    , sDescription
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Requests Amazon API Gateway to get information about a Stage resource.
--
-- /See:/ 'getStage' smart constructor.
data GetStage = GetStage'
    { _gssRestAPIId :: !Text
    , _gssStageName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetStage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gssRestAPIId'
--
-- * 'gssStageName'
getStage
    :: Text -- ^ 'gssRestAPIId'
    -> Text -- ^ 'gssStageName'
    -> GetStage
getStage pRestAPIId_ pStageName_ =
    GetStage'
    { _gssRestAPIId = pRestAPIId_
    , _gssStageName = pStageName_
    }

-- | The identifier of the RestApi resource for the Stage resource to get
-- information about.
gssRestAPIId :: Lens' GetStage Text
gssRestAPIId = lens _gssRestAPIId (\ s a -> s{_gssRestAPIId = a});

-- | The name of the Stage resource to get information about.
gssStageName :: Lens' GetStage Text
gssStageName = lens _gssStageName (\ s a -> s{_gssStageName = a});

instance AWSRequest GetStage where
        type Rs GetStage = Stage
        request = get aPIGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders GetStage where
        toHeaders = const mempty

instance ToPath GetStage where
        toPath GetStage'{..}
          = mconcat
              ["/restapis/", toBS _gssRestAPIId, "/stages/",
               toBS _gssStageName]

instance ToQuery GetStage where
        toQuery = const mempty
