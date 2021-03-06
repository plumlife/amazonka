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
-- Module      : Network.AWS.APIGateway.DeleteClientCertificate
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
--
-- /See:/ <http://docs.aws.amazon.com/apigateway/api-reference/resource/DeleteClientCertificate.html AWS API Reference> for DeleteClientCertificate.
module Network.AWS.APIGateway.DeleteClientCertificate
    (
    -- * Creating a Request
      deleteClientCertificate
    , DeleteClientCertificate
    -- * Request Lenses
    , dccClientCertificateId

    -- * Destructuring the Response
    , deleteClientCertificateResponse
    , DeleteClientCertificateResponse
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteClientCertificate' smart constructor.
newtype DeleteClientCertificate = DeleteClientCertificate'
    { _dccClientCertificateId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteClientCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dccClientCertificateId'
deleteClientCertificate
    :: Text -- ^ 'dccClientCertificateId'
    -> DeleteClientCertificate
deleteClientCertificate pClientCertificateId_ =
    DeleteClientCertificate'
    { _dccClientCertificateId = pClientCertificateId_
    }

-- | Undocumented member.
dccClientCertificateId :: Lens' DeleteClientCertificate Text
dccClientCertificateId = lens _dccClientCertificateId (\ s a -> s{_dccClientCertificateId = a});

instance AWSRequest DeleteClientCertificate where
        type Rs DeleteClientCertificate =
             DeleteClientCertificateResponse
        request = delete aPIGateway
        response
          = receiveNull DeleteClientCertificateResponse'

instance ToHeaders DeleteClientCertificate where
        toHeaders = const mempty

instance ToPath DeleteClientCertificate where
        toPath DeleteClientCertificate'{..}
          = mconcat
              ["/clientcertificates/",
               toBS _dccClientCertificateId]

instance ToQuery DeleteClientCertificate where
        toQuery = const mempty

-- | /See:/ 'deleteClientCertificateResponse' smart constructor.
data DeleteClientCertificateResponse =
    DeleteClientCertificateResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteClientCertificateResponse' with the minimum fields required to make a request.
--
deleteClientCertificateResponse
    :: DeleteClientCertificateResponse
deleteClientCertificateResponse = DeleteClientCertificateResponse'
