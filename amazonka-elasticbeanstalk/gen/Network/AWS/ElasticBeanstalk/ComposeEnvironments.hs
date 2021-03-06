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
-- Module      : Network.AWS.ElasticBeanstalk.ComposeEnvironments
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create or update a group of environments that each run a separate
-- component of a single application. Takes a list of version labels that
-- specify application source bundles for each of the environments to
-- create or update. The name of each environment and other required
-- information must be included in the source bundles in an environment
-- manifest named 'env.yaml'. See
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-mgmt-compose.html Compose Environments>
-- for details.
--
-- /See:/ <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_ComposeEnvironments.html AWS API Reference> for ComposeEnvironments.
module Network.AWS.ElasticBeanstalk.ComposeEnvironments
    (
    -- * Creating a Request
      composeEnvironments
    , ComposeEnvironments
    -- * Request Lenses
    , ceVersionLabels
    , ceApplicationName
    , ceGroupName

    -- * Destructuring the Response
    , environmentDescriptionsMessage
    , EnvironmentDescriptionsMessage
    -- * Response Lenses
    , edmEnvironments
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.ElasticBeanstalk.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'composeEnvironments' smart constructor.
data ComposeEnvironments = ComposeEnvironments'
    { _ceVersionLabels   :: !(Maybe [Text])
    , _ceApplicationName :: !(Maybe Text)
    , _ceGroupName       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ComposeEnvironments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceVersionLabels'
--
-- * 'ceApplicationName'
--
-- * 'ceGroupName'
composeEnvironments
    :: ComposeEnvironments
composeEnvironments =
    ComposeEnvironments'
    { _ceVersionLabels = Nothing
    , _ceApplicationName = Nothing
    , _ceGroupName = Nothing
    }

-- | A list of version labels, specifying one or more application source
-- bundles that belong to the target application. Each source bundle must
-- include an environment manifest that specifies the name of the
-- environment and the name of the solution stack to use, and optionally
-- can specify environment links to create.
ceVersionLabels :: Lens' ComposeEnvironments [Text]
ceVersionLabels = lens _ceVersionLabels (\ s a -> s{_ceVersionLabels = a}) . _Default . _Coerce;

-- | The name of the application to which the specified source bundles
-- belong.
ceApplicationName :: Lens' ComposeEnvironments (Maybe Text)
ceApplicationName = lens _ceApplicationName (\ s a -> s{_ceApplicationName = a});

-- | The name of the group to which the target environments belong. Specify a
-- group name only if the environment name defined in each target
-- environment\'s manifest ends with a + (plus) character. See
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-mgmt-compose.html#environment-mgmt-compose-envyaml Environment Manifest (env.yaml)>
-- for details.
ceGroupName :: Lens' ComposeEnvironments (Maybe Text)
ceGroupName = lens _ceGroupName (\ s a -> s{_ceGroupName = a});

instance AWSRequest ComposeEnvironments where
        type Rs ComposeEnvironments =
             EnvironmentDescriptionsMessage
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper "ComposeEnvironmentsResult"
              (\ s h x -> parseXML x)

instance ToHeaders ComposeEnvironments where
        toHeaders = const mempty

instance ToPath ComposeEnvironments where
        toPath = const "/"

instance ToQuery ComposeEnvironments where
        toQuery ComposeEnvironments'{..}
          = mconcat
              ["Action" =: ("ComposeEnvironments" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "VersionLabels" =:
                 toQuery (toQueryList "member" <$> _ceVersionLabels),
               "ApplicationName" =: _ceApplicationName,
               "GroupName" =: _ceGroupName]
