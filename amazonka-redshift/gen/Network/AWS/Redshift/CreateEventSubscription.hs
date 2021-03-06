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
-- Module      : Network.AWS.Redshift.CreateEventSubscription
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Redshift event notification subscription. This action
-- requires an ARN (Amazon Resource Name) of an Amazon SNS topic created by
-- either the Amazon Redshift console, the Amazon SNS console, or the
-- Amazon SNS API. To obtain an ARN with Amazon SNS, you must create a
-- topic in Amazon SNS and subscribe to the topic. The ARN is displayed in
-- the SNS console.
--
-- You can specify the source type, and lists of Amazon Redshift source
-- IDs, event categories, and event severities. Notifications will be sent
-- for all events you want that match those criteria. For example, you can
-- specify source type = cluster, source ID = my-cluster-1 and mycluster2,
-- event categories = Availability, Backup, and severity = ERROR. The
-- subscription will only send notifications for those ERROR events in the
-- Availability and Backup categories for the specified clusters.
--
-- If you specify both the source type and source IDs, such as source type
-- = cluster and source identifier = my-cluster-1, notifications will be
-- sent for all the cluster events for my-cluster-1. If you specify a
-- source type but do not specify a source identifier, you will receive
-- notice of the events for the objects of that type in your AWS account.
-- If you do not specify either the SourceType nor the SourceIdentifier,
-- you will be notified of events generated from all Amazon Redshift
-- sources belonging to your AWS account. You must specify a source type if
-- you specify a source ID.
--
-- /See:/ <http://docs.aws.amazon.com/redshift/latest/APIReference/API_CreateEventSubscription.html AWS API Reference> for CreateEventSubscription.
module Network.AWS.Redshift.CreateEventSubscription
    (
    -- * Creating a Request
      createEventSubscription
    , CreateEventSubscription
    -- * Request Lenses
    , cesEnabled
    , cesSourceType
    , cesSeverity
    , cesEventCategories
    , cesSourceIds
    , cesTags
    , cesSubscriptionName
    , cesSNSTopicARN

    -- * Destructuring the Response
    , createEventSubscriptionResponse
    , CreateEventSubscriptionResponse
    -- * Response Lenses
    , cesrsEventSubscription
    , cesrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Redshift.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'createEventSubscription' smart constructor.
data CreateEventSubscription = CreateEventSubscription'
    { _cesEnabled          :: !(Maybe Bool)
    , _cesSourceType       :: !(Maybe Text)
    , _cesSeverity         :: !(Maybe Text)
    , _cesEventCategories  :: !(Maybe [Text])
    , _cesSourceIds        :: !(Maybe [Text])
    , _cesTags             :: !(Maybe [Tag])
    , _cesSubscriptionName :: !Text
    , _cesSNSTopicARN      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateEventSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cesEnabled'
--
-- * 'cesSourceType'
--
-- * 'cesSeverity'
--
-- * 'cesEventCategories'
--
-- * 'cesSourceIds'
--
-- * 'cesTags'
--
-- * 'cesSubscriptionName'
--
-- * 'cesSNSTopicARN'
createEventSubscription
    :: Text -- ^ 'cesSubscriptionName'
    -> Text -- ^ 'cesSNSTopicARN'
    -> CreateEventSubscription
createEventSubscription pSubscriptionName_ pSNSTopicARN_ =
    CreateEventSubscription'
    { _cesEnabled = Nothing
    , _cesSourceType = Nothing
    , _cesSeverity = Nothing
    , _cesEventCategories = Nothing
    , _cesSourceIds = Nothing
    , _cesTags = Nothing
    , _cesSubscriptionName = pSubscriptionName_
    , _cesSNSTopicARN = pSNSTopicARN_
    }

-- | A Boolean value; set to 'true' to activate the subscription, set to
-- 'false' to create the subscription but not active it.
cesEnabled :: Lens' CreateEventSubscription (Maybe Bool)
cesEnabled = lens _cesEnabled (\ s a -> s{_cesEnabled = a});

-- | The type of source that will be generating the events. For example, if
-- you want to be notified of events generated by a cluster, you would set
-- this parameter to cluster. If this value is not specified, events are
-- returned for all Amazon Redshift objects in your AWS account. You must
-- specify a source type in order to specify source IDs.
--
-- Valid values: cluster, cluster-parameter-group, cluster-security-group,
-- and cluster-snapshot.
cesSourceType :: Lens' CreateEventSubscription (Maybe Text)
cesSourceType = lens _cesSourceType (\ s a -> s{_cesSourceType = a});

-- | Specifies the Amazon Redshift event severity to be published by the
-- event notification subscription.
--
-- Values: ERROR, INFO
cesSeverity :: Lens' CreateEventSubscription (Maybe Text)
cesSeverity = lens _cesSeverity (\ s a -> s{_cesSeverity = a});

-- | Specifies the Amazon Redshift event categories to be published by the
-- event notification subscription.
--
-- Values: Configuration, Management, Monitoring, Security
cesEventCategories :: Lens' CreateEventSubscription [Text]
cesEventCategories = lens _cesEventCategories (\ s a -> s{_cesEventCategories = a}) . _Default . _Coerce;

-- | A list of one or more identifiers of Amazon Redshift source objects. All
-- of the objects must be of the same type as was specified in the source
-- type parameter. The event subscription will return only events generated
-- by the specified objects. If not specified, then events are returned for
-- all objects within the source type specified.
--
-- Example: my-cluster-1, my-cluster-2
--
-- Example: my-snapshot-20131010
cesSourceIds :: Lens' CreateEventSubscription [Text]
cesSourceIds = lens _cesSourceIds (\ s a -> s{_cesSourceIds = a}) . _Default . _Coerce;

-- | A list of tag instances.
cesTags :: Lens' CreateEventSubscription [Tag]
cesTags = lens _cesTags (\ s a -> s{_cesTags = a}) . _Default . _Coerce;

-- | The name of the event subscription to be created.
--
-- Constraints:
--
-- -   Cannot be null, empty, or blank.
-- -   Must contain from 1 to 255 alphanumeric characters or hyphens.
-- -   First character must be a letter.
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
cesSubscriptionName :: Lens' CreateEventSubscription Text
cesSubscriptionName = lens _cesSubscriptionName (\ s a -> s{_cesSubscriptionName = a});

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic used to transmit
-- the event notifications. The ARN is created by Amazon SNS when you
-- create a topic and subscribe to it.
cesSNSTopicARN :: Lens' CreateEventSubscription Text
cesSNSTopicARN = lens _cesSNSTopicARN (\ s a -> s{_cesSNSTopicARN = a});

instance AWSRequest CreateEventSubscription where
        type Rs CreateEventSubscription =
             CreateEventSubscriptionResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper "CreateEventSubscriptionResult"
              (\ s h x ->
                 CreateEventSubscriptionResponse' <$>
                   (x .@? "EventSubscription") <*> (pure (fromEnum s)))

instance ToHeaders CreateEventSubscription where
        toHeaders = const mempty

instance ToPath CreateEventSubscription where
        toPath = const "/"

instance ToQuery CreateEventSubscription where
        toQuery CreateEventSubscription'{..}
          = mconcat
              ["Action" =:
                 ("CreateEventSubscription" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "Enabled" =: _cesEnabled,
               "SourceType" =: _cesSourceType,
               "Severity" =: _cesSeverity,
               "EventCategories" =:
                 toQuery
                   (toQueryList "EventCategory" <$>
                      _cesEventCategories),
               "SourceIds" =:
                 toQuery (toQueryList "SourceId" <$> _cesSourceIds),
               "Tags" =: toQuery (toQueryList "Tag" <$> _cesTags),
               "SubscriptionName" =: _cesSubscriptionName,
               "SnsTopicArn" =: _cesSNSTopicARN]

-- | /See:/ 'createEventSubscriptionResponse' smart constructor.
data CreateEventSubscriptionResponse = CreateEventSubscriptionResponse'
    { _cesrsEventSubscription :: !(Maybe EventSubscription)
    , _cesrsResponseStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateEventSubscriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cesrsEventSubscription'
--
-- * 'cesrsResponseStatus'
createEventSubscriptionResponse
    :: Int -- ^ 'cesrsResponseStatus'
    -> CreateEventSubscriptionResponse
createEventSubscriptionResponse pResponseStatus_ =
    CreateEventSubscriptionResponse'
    { _cesrsEventSubscription = Nothing
    , _cesrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
cesrsEventSubscription :: Lens' CreateEventSubscriptionResponse (Maybe EventSubscription)
cesrsEventSubscription = lens _cesrsEventSubscription (\ s a -> s{_cesrsEventSubscription = a});

-- | The response status code.
cesrsResponseStatus :: Lens' CreateEventSubscriptionResponse Int
cesrsResponseStatus = lens _cesrsResponseStatus (\ s a -> s{_cesrsResponseStatus = a});
