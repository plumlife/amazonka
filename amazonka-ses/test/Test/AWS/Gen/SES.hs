{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SES
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.SES where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.SES
import Test.AWS.SES.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testCreateReceiptRuleSet $
--             createReceiptRuleSet
--
--         , testGetSendQuota $
--             getSendQuota
--
--         , testPutIdentityPolicy $
--             putIdentityPolicy
--
--         , testDeleteIdentityPolicy $
--             deleteIdentityPolicy
--
--         , testGetIdentityNotificationAttributes $
--             getIdentityNotificationAttributes
--
--         , testListIdentityPolicies $
--             listIdentityPolicies
--
--         , testSetIdentityDkimEnabled $
--             setIdentityDkimEnabled
--
--         , testListReceiptFilters $
--             listReceiptFilters
--
--         , testDescribeReceiptRuleSet $
--             describeReceiptRuleSet
--
--         , testCreateReceiptFilter $
--             createReceiptFilter
--
--         , testSetIdentityFeedbackForwardingEnabled $
--             setIdentityFeedbackForwardingEnabled
--
--         , testGetIdentityVerificationAttributes $
--             getIdentityVerificationAttributes
--
--         , testGetIdentityPolicies $
--             getIdentityPolicies
--
--         , testVerifyDomainIdentity $
--             verifyDomainIdentity
--
--         , testReorderReceiptRuleSet $
--             reorderReceiptRuleSet
--
--         , testListReceiptRuleSets $
--             listReceiptRuleSets
--
--         , testDeleteReceiptRuleSet $
--             deleteReceiptRuleSet
--
--         , testSetReceiptRulePosition $
--             setReceiptRulePosition
--
--         , testSendBounce $
--             sendBounce
--
--         , testGetIdentityDkimAttributes $
--             getIdentityDkimAttributes
--
--         , testVerifyDomainDkim $
--             verifyDomainDkim
--
--         , testSendRawEmail $
--             sendRawEmail
--
--         , testGetSendStatistics $
--             getSendStatistics
--
--         , testDeleteIdentity $
--             deleteIdentity
--
--         , testDescribeReceiptRule $
--             describeReceiptRule
--
--         , testListIdentities $
--             listIdentities
--
--         , testVerifyEmailIdentity $
--             verifyEmailIdentity
--
--         , testVerifyEmailAddress $
--             verifyEmailAddress
--
--         , testDeleteVerifiedEmailAddress $
--             deleteVerifiedEmailAddress
--
--         , testDeleteReceiptFilter $
--             deleteReceiptFilter
--
--         , testListVerifiedEmailAddresses $
--             listVerifiedEmailAddresses
--
--         , testSetIdentityNotificationTopic $
--             setIdentityNotificationTopic
--
--         , testSendEmail $
--             sendEmail
--
--         , testDeleteReceiptRule $
--             deleteReceiptRule
--
--         , testUpdateReceiptRule $
--             updateReceiptRule
--
--         , testCloneReceiptRuleSet $
--             cloneReceiptRuleSet
--
--         , testCreateReceiptRule $
--             createReceiptRule
--
--         , testSetActiveReceiptRuleSet $
--             setActiveReceiptRuleSet
--
--         , testDescribeActiveReceiptRuleSet $
--             describeActiveReceiptRuleSet
--
--           ]

--     , testGroup "response"
--         [ testCreateReceiptRuleSetResponse $
--             createReceiptRuleSetResponse
--
--         , testGetSendQuotaResponse $
--             getSendQuotaResponse
--
--         , testPutIdentityPolicyResponse $
--             putIdentityPolicyResponse
--
--         , testDeleteIdentityPolicyResponse $
--             deleteIdentityPolicyResponse
--
--         , testGetIdentityNotificationAttributesResponse $
--             getIdentityNotificationAttributesResponse
--
--         , testListIdentityPoliciesResponse $
--             listIdentityPoliciesResponse
--
--         , testSetIdentityDkimEnabledResponse $
--             setIdentityDkimEnabledResponse
--
--         , testListReceiptFiltersResponse $
--             listReceiptFiltersResponse
--
--         , testDescribeReceiptRuleSetResponse $
--             describeReceiptRuleSetResponse
--
--         , testCreateReceiptFilterResponse $
--             createReceiptFilterResponse
--
--         , testSetIdentityFeedbackForwardingEnabledResponse $
--             setIdentityFeedbackForwardingEnabledResponse
--
--         , testGetIdentityVerificationAttributesResponse $
--             getIdentityVerificationAttributesResponse
--
--         , testGetIdentityPoliciesResponse $
--             getIdentityPoliciesResponse
--
--         , testVerifyDomainIdentityResponse $
--             verifyDomainIdentityResponse
--
--         , testReorderReceiptRuleSetResponse $
--             reorderReceiptRuleSetResponse
--
--         , testListReceiptRuleSetsResponse $
--             listReceiptRuleSetsResponse
--
--         , testDeleteReceiptRuleSetResponse $
--             deleteReceiptRuleSetResponse
--
--         , testSetReceiptRulePositionResponse $
--             setReceiptRulePositionResponse
--
--         , testSendBounceResponse $
--             sendBounceResponse
--
--         , testGetIdentityDkimAttributesResponse $
--             getIdentityDkimAttributesResponse
--
--         , testVerifyDomainDkimResponse $
--             verifyDomainDkimResponse
--
--         , testSendRawEmailResponse $
--             sendRawEmailResponse
--
--         , testGetSendStatisticsResponse $
--             getSendStatisticsResponse
--
--         , testDeleteIdentityResponse $
--             deleteIdentityResponse
--
--         , testDescribeReceiptRuleResponse $
--             describeReceiptRuleResponse
--
--         , testListIdentitiesResponse $
--             listIdentitiesResponse
--
--         , testVerifyEmailIdentityResponse $
--             verifyEmailIdentityResponse
--
--         , testVerifyEmailAddressResponse $
--             verifyEmailAddressResponse
--
--         , testDeleteVerifiedEmailAddressResponse $
--             deleteVerifiedEmailAddressResponse
--
--         , testDeleteReceiptFilterResponse $
--             deleteReceiptFilterResponse
--
--         , testListVerifiedEmailAddressesResponse $
--             listVerifiedEmailAddressesResponse
--
--         , testSetIdentityNotificationTopicResponse $
--             setIdentityNotificationTopicResponse
--
--         , testSendEmailResponse $
--             sendEmailResponse
--
--         , testDeleteReceiptRuleResponse $
--             deleteReceiptRuleResponse
--
--         , testUpdateReceiptRuleResponse $
--             updateReceiptRuleResponse
--
--         , testCloneReceiptRuleSetResponse $
--             cloneReceiptRuleSetResponse
--
--         , testCreateReceiptRuleResponse $
--             createReceiptRuleResponse
--
--         , testSetActiveReceiptRuleSetResponse $
--             setActiveReceiptRuleSetResponse
--
--         , testDescribeActiveReceiptRuleSetResponse $
--             describeActiveReceiptRuleSetResponse
--
--           ]
--     ]

-- Requests

testCreateReceiptRuleSet :: CreateReceiptRuleSet -> TestTree
testCreateReceiptRuleSet = req
    "CreateReceiptRuleSet"
    "fixture/CreateReceiptRuleSet.yaml"

testGetSendQuota :: GetSendQuota -> TestTree
testGetSendQuota = req
    "GetSendQuota"
    "fixture/GetSendQuota.yaml"

testPutIdentityPolicy :: PutIdentityPolicy -> TestTree
testPutIdentityPolicy = req
    "PutIdentityPolicy"
    "fixture/PutIdentityPolicy.yaml"

testDeleteIdentityPolicy :: DeleteIdentityPolicy -> TestTree
testDeleteIdentityPolicy = req
    "DeleteIdentityPolicy"
    "fixture/DeleteIdentityPolicy.yaml"

testGetIdentityNotificationAttributes :: GetIdentityNotificationAttributes -> TestTree
testGetIdentityNotificationAttributes = req
    "GetIdentityNotificationAttributes"
    "fixture/GetIdentityNotificationAttributes.yaml"

testListIdentityPolicies :: ListIdentityPolicies -> TestTree
testListIdentityPolicies = req
    "ListIdentityPolicies"
    "fixture/ListIdentityPolicies.yaml"

testSetIdentityDkimEnabled :: SetIdentityDkimEnabled -> TestTree
testSetIdentityDkimEnabled = req
    "SetIdentityDkimEnabled"
    "fixture/SetIdentityDkimEnabled.yaml"

testListReceiptFilters :: ListReceiptFilters -> TestTree
testListReceiptFilters = req
    "ListReceiptFilters"
    "fixture/ListReceiptFilters.yaml"

testDescribeReceiptRuleSet :: DescribeReceiptRuleSet -> TestTree
testDescribeReceiptRuleSet = req
    "DescribeReceiptRuleSet"
    "fixture/DescribeReceiptRuleSet.yaml"

testCreateReceiptFilter :: CreateReceiptFilter -> TestTree
testCreateReceiptFilter = req
    "CreateReceiptFilter"
    "fixture/CreateReceiptFilter.yaml"

testSetIdentityFeedbackForwardingEnabled :: SetIdentityFeedbackForwardingEnabled -> TestTree
testSetIdentityFeedbackForwardingEnabled = req
    "SetIdentityFeedbackForwardingEnabled"
    "fixture/SetIdentityFeedbackForwardingEnabled.yaml"

testGetIdentityVerificationAttributes :: GetIdentityVerificationAttributes -> TestTree
testGetIdentityVerificationAttributes = req
    "GetIdentityVerificationAttributes"
    "fixture/GetIdentityVerificationAttributes.yaml"

testGetIdentityPolicies :: GetIdentityPolicies -> TestTree
testGetIdentityPolicies = req
    "GetIdentityPolicies"
    "fixture/GetIdentityPolicies.yaml"

testVerifyDomainIdentity :: VerifyDomainIdentity -> TestTree
testVerifyDomainIdentity = req
    "VerifyDomainIdentity"
    "fixture/VerifyDomainIdentity.yaml"

testReorderReceiptRuleSet :: ReorderReceiptRuleSet -> TestTree
testReorderReceiptRuleSet = req
    "ReorderReceiptRuleSet"
    "fixture/ReorderReceiptRuleSet.yaml"

testListReceiptRuleSets :: ListReceiptRuleSets -> TestTree
testListReceiptRuleSets = req
    "ListReceiptRuleSets"
    "fixture/ListReceiptRuleSets.yaml"

testDeleteReceiptRuleSet :: DeleteReceiptRuleSet -> TestTree
testDeleteReceiptRuleSet = req
    "DeleteReceiptRuleSet"
    "fixture/DeleteReceiptRuleSet.yaml"

testSetReceiptRulePosition :: SetReceiptRulePosition -> TestTree
testSetReceiptRulePosition = req
    "SetReceiptRulePosition"
    "fixture/SetReceiptRulePosition.yaml"

testSendBounce :: SendBounce -> TestTree
testSendBounce = req
    "SendBounce"
    "fixture/SendBounce.yaml"

testGetIdentityDkimAttributes :: GetIdentityDkimAttributes -> TestTree
testGetIdentityDkimAttributes = req
    "GetIdentityDkimAttributes"
    "fixture/GetIdentityDkimAttributes.yaml"

testVerifyDomainDkim :: VerifyDomainDkim -> TestTree
testVerifyDomainDkim = req
    "VerifyDomainDkim"
    "fixture/VerifyDomainDkim.yaml"

testSendRawEmail :: SendRawEmail -> TestTree
testSendRawEmail = req
    "SendRawEmail"
    "fixture/SendRawEmail.yaml"

testGetSendStatistics :: GetSendStatistics -> TestTree
testGetSendStatistics = req
    "GetSendStatistics"
    "fixture/GetSendStatistics.yaml"

testDeleteIdentity :: DeleteIdentity -> TestTree
testDeleteIdentity = req
    "DeleteIdentity"
    "fixture/DeleteIdentity.yaml"

testDescribeReceiptRule :: DescribeReceiptRule -> TestTree
testDescribeReceiptRule = req
    "DescribeReceiptRule"
    "fixture/DescribeReceiptRule.yaml"

testListIdentities :: ListIdentities -> TestTree
testListIdentities = req
    "ListIdentities"
    "fixture/ListIdentities.yaml"

testVerifyEmailIdentity :: VerifyEmailIdentity -> TestTree
testVerifyEmailIdentity = req
    "VerifyEmailIdentity"
    "fixture/VerifyEmailIdentity.yaml"

testVerifyEmailAddress :: VerifyEmailAddress -> TestTree
testVerifyEmailAddress = req
    "VerifyEmailAddress"
    "fixture/VerifyEmailAddress.yaml"

testDeleteVerifiedEmailAddress :: DeleteVerifiedEmailAddress -> TestTree
testDeleteVerifiedEmailAddress = req
    "DeleteVerifiedEmailAddress"
    "fixture/DeleteVerifiedEmailAddress.yaml"

testDeleteReceiptFilter :: DeleteReceiptFilter -> TestTree
testDeleteReceiptFilter = req
    "DeleteReceiptFilter"
    "fixture/DeleteReceiptFilter.yaml"

testListVerifiedEmailAddresses :: ListVerifiedEmailAddresses -> TestTree
testListVerifiedEmailAddresses = req
    "ListVerifiedEmailAddresses"
    "fixture/ListVerifiedEmailAddresses.yaml"

testSetIdentityNotificationTopic :: SetIdentityNotificationTopic -> TestTree
testSetIdentityNotificationTopic = req
    "SetIdentityNotificationTopic"
    "fixture/SetIdentityNotificationTopic.yaml"

testSendEmail :: SendEmail -> TestTree
testSendEmail = req
    "SendEmail"
    "fixture/SendEmail.yaml"

testDeleteReceiptRule :: DeleteReceiptRule -> TestTree
testDeleteReceiptRule = req
    "DeleteReceiptRule"
    "fixture/DeleteReceiptRule.yaml"

testUpdateReceiptRule :: UpdateReceiptRule -> TestTree
testUpdateReceiptRule = req
    "UpdateReceiptRule"
    "fixture/UpdateReceiptRule.yaml"

testCloneReceiptRuleSet :: CloneReceiptRuleSet -> TestTree
testCloneReceiptRuleSet = req
    "CloneReceiptRuleSet"
    "fixture/CloneReceiptRuleSet.yaml"

testCreateReceiptRule :: CreateReceiptRule -> TestTree
testCreateReceiptRule = req
    "CreateReceiptRule"
    "fixture/CreateReceiptRule.yaml"

testSetActiveReceiptRuleSet :: SetActiveReceiptRuleSet -> TestTree
testSetActiveReceiptRuleSet = req
    "SetActiveReceiptRuleSet"
    "fixture/SetActiveReceiptRuleSet.yaml"

testDescribeActiveReceiptRuleSet :: DescribeActiveReceiptRuleSet -> TestTree
testDescribeActiveReceiptRuleSet = req
    "DescribeActiveReceiptRuleSet"
    "fixture/DescribeActiveReceiptRuleSet.yaml"

-- Responses

testCreateReceiptRuleSetResponse :: CreateReceiptRuleSetResponse -> TestTree
testCreateReceiptRuleSetResponse = res
    "CreateReceiptRuleSetResponse"
    "fixture/CreateReceiptRuleSetResponse.proto"
    sES
    (Proxy :: Proxy CreateReceiptRuleSet)

testGetSendQuotaResponse :: GetSendQuotaResponse -> TestTree
testGetSendQuotaResponse = res
    "GetSendQuotaResponse"
    "fixture/GetSendQuotaResponse.proto"
    sES
    (Proxy :: Proxy GetSendQuota)

testPutIdentityPolicyResponse :: PutIdentityPolicyResponse -> TestTree
testPutIdentityPolicyResponse = res
    "PutIdentityPolicyResponse"
    "fixture/PutIdentityPolicyResponse.proto"
    sES
    (Proxy :: Proxy PutIdentityPolicy)

testDeleteIdentityPolicyResponse :: DeleteIdentityPolicyResponse -> TestTree
testDeleteIdentityPolicyResponse = res
    "DeleteIdentityPolicyResponse"
    "fixture/DeleteIdentityPolicyResponse.proto"
    sES
    (Proxy :: Proxy DeleteIdentityPolicy)

testGetIdentityNotificationAttributesResponse :: GetIdentityNotificationAttributesResponse -> TestTree
testGetIdentityNotificationAttributesResponse = res
    "GetIdentityNotificationAttributesResponse"
    "fixture/GetIdentityNotificationAttributesResponse.proto"
    sES
    (Proxy :: Proxy GetIdentityNotificationAttributes)

testListIdentityPoliciesResponse :: ListIdentityPoliciesResponse -> TestTree
testListIdentityPoliciesResponse = res
    "ListIdentityPoliciesResponse"
    "fixture/ListIdentityPoliciesResponse.proto"
    sES
    (Proxy :: Proxy ListIdentityPolicies)

testSetIdentityDkimEnabledResponse :: SetIdentityDkimEnabledResponse -> TestTree
testSetIdentityDkimEnabledResponse = res
    "SetIdentityDkimEnabledResponse"
    "fixture/SetIdentityDkimEnabledResponse.proto"
    sES
    (Proxy :: Proxy SetIdentityDkimEnabled)

testListReceiptFiltersResponse :: ListReceiptFiltersResponse -> TestTree
testListReceiptFiltersResponse = res
    "ListReceiptFiltersResponse"
    "fixture/ListReceiptFiltersResponse.proto"
    sES
    (Proxy :: Proxy ListReceiptFilters)

testDescribeReceiptRuleSetResponse :: DescribeReceiptRuleSetResponse -> TestTree
testDescribeReceiptRuleSetResponse = res
    "DescribeReceiptRuleSetResponse"
    "fixture/DescribeReceiptRuleSetResponse.proto"
    sES
    (Proxy :: Proxy DescribeReceiptRuleSet)

testCreateReceiptFilterResponse :: CreateReceiptFilterResponse -> TestTree
testCreateReceiptFilterResponse = res
    "CreateReceiptFilterResponse"
    "fixture/CreateReceiptFilterResponse.proto"
    sES
    (Proxy :: Proxy CreateReceiptFilter)

testSetIdentityFeedbackForwardingEnabledResponse :: SetIdentityFeedbackForwardingEnabledResponse -> TestTree
testSetIdentityFeedbackForwardingEnabledResponse = res
    "SetIdentityFeedbackForwardingEnabledResponse"
    "fixture/SetIdentityFeedbackForwardingEnabledResponse.proto"
    sES
    (Proxy :: Proxy SetIdentityFeedbackForwardingEnabled)

testGetIdentityVerificationAttributesResponse :: GetIdentityVerificationAttributesResponse -> TestTree
testGetIdentityVerificationAttributesResponse = res
    "GetIdentityVerificationAttributesResponse"
    "fixture/GetIdentityVerificationAttributesResponse.proto"
    sES
    (Proxy :: Proxy GetIdentityVerificationAttributes)

testGetIdentityPoliciesResponse :: GetIdentityPoliciesResponse -> TestTree
testGetIdentityPoliciesResponse = res
    "GetIdentityPoliciesResponse"
    "fixture/GetIdentityPoliciesResponse.proto"
    sES
    (Proxy :: Proxy GetIdentityPolicies)

testVerifyDomainIdentityResponse :: VerifyDomainIdentityResponse -> TestTree
testVerifyDomainIdentityResponse = res
    "VerifyDomainIdentityResponse"
    "fixture/VerifyDomainIdentityResponse.proto"
    sES
    (Proxy :: Proxy VerifyDomainIdentity)

testReorderReceiptRuleSetResponse :: ReorderReceiptRuleSetResponse -> TestTree
testReorderReceiptRuleSetResponse = res
    "ReorderReceiptRuleSetResponse"
    "fixture/ReorderReceiptRuleSetResponse.proto"
    sES
    (Proxy :: Proxy ReorderReceiptRuleSet)

testListReceiptRuleSetsResponse :: ListReceiptRuleSetsResponse -> TestTree
testListReceiptRuleSetsResponse = res
    "ListReceiptRuleSetsResponse"
    "fixture/ListReceiptRuleSetsResponse.proto"
    sES
    (Proxy :: Proxy ListReceiptRuleSets)

testDeleteReceiptRuleSetResponse :: DeleteReceiptRuleSetResponse -> TestTree
testDeleteReceiptRuleSetResponse = res
    "DeleteReceiptRuleSetResponse"
    "fixture/DeleteReceiptRuleSetResponse.proto"
    sES
    (Proxy :: Proxy DeleteReceiptRuleSet)

testSetReceiptRulePositionResponse :: SetReceiptRulePositionResponse -> TestTree
testSetReceiptRulePositionResponse = res
    "SetReceiptRulePositionResponse"
    "fixture/SetReceiptRulePositionResponse.proto"
    sES
    (Proxy :: Proxy SetReceiptRulePosition)

testSendBounceResponse :: SendBounceResponse -> TestTree
testSendBounceResponse = res
    "SendBounceResponse"
    "fixture/SendBounceResponse.proto"
    sES
    (Proxy :: Proxy SendBounce)

testGetIdentityDkimAttributesResponse :: GetIdentityDkimAttributesResponse -> TestTree
testGetIdentityDkimAttributesResponse = res
    "GetIdentityDkimAttributesResponse"
    "fixture/GetIdentityDkimAttributesResponse.proto"
    sES
    (Proxy :: Proxy GetIdentityDkimAttributes)

testVerifyDomainDkimResponse :: VerifyDomainDkimResponse -> TestTree
testVerifyDomainDkimResponse = res
    "VerifyDomainDkimResponse"
    "fixture/VerifyDomainDkimResponse.proto"
    sES
    (Proxy :: Proxy VerifyDomainDkim)

testSendRawEmailResponse :: SendRawEmailResponse -> TestTree
testSendRawEmailResponse = res
    "SendRawEmailResponse"
    "fixture/SendRawEmailResponse.proto"
    sES
    (Proxy :: Proxy SendRawEmail)

testGetSendStatisticsResponse :: GetSendStatisticsResponse -> TestTree
testGetSendStatisticsResponse = res
    "GetSendStatisticsResponse"
    "fixture/GetSendStatisticsResponse.proto"
    sES
    (Proxy :: Proxy GetSendStatistics)

testDeleteIdentityResponse :: DeleteIdentityResponse -> TestTree
testDeleteIdentityResponse = res
    "DeleteIdentityResponse"
    "fixture/DeleteIdentityResponse.proto"
    sES
    (Proxy :: Proxy DeleteIdentity)

testDescribeReceiptRuleResponse :: DescribeReceiptRuleResponse -> TestTree
testDescribeReceiptRuleResponse = res
    "DescribeReceiptRuleResponse"
    "fixture/DescribeReceiptRuleResponse.proto"
    sES
    (Proxy :: Proxy DescribeReceiptRule)

testListIdentitiesResponse :: ListIdentitiesResponse -> TestTree
testListIdentitiesResponse = res
    "ListIdentitiesResponse"
    "fixture/ListIdentitiesResponse.proto"
    sES
    (Proxy :: Proxy ListIdentities)

testVerifyEmailIdentityResponse :: VerifyEmailIdentityResponse -> TestTree
testVerifyEmailIdentityResponse = res
    "VerifyEmailIdentityResponse"
    "fixture/VerifyEmailIdentityResponse.proto"
    sES
    (Proxy :: Proxy VerifyEmailIdentity)

testVerifyEmailAddressResponse :: VerifyEmailAddressResponse -> TestTree
testVerifyEmailAddressResponse = res
    "VerifyEmailAddressResponse"
    "fixture/VerifyEmailAddressResponse.proto"
    sES
    (Proxy :: Proxy VerifyEmailAddress)

testDeleteVerifiedEmailAddressResponse :: DeleteVerifiedEmailAddressResponse -> TestTree
testDeleteVerifiedEmailAddressResponse = res
    "DeleteVerifiedEmailAddressResponse"
    "fixture/DeleteVerifiedEmailAddressResponse.proto"
    sES
    (Proxy :: Proxy DeleteVerifiedEmailAddress)

testDeleteReceiptFilterResponse :: DeleteReceiptFilterResponse -> TestTree
testDeleteReceiptFilterResponse = res
    "DeleteReceiptFilterResponse"
    "fixture/DeleteReceiptFilterResponse.proto"
    sES
    (Proxy :: Proxy DeleteReceiptFilter)

testListVerifiedEmailAddressesResponse :: ListVerifiedEmailAddressesResponse -> TestTree
testListVerifiedEmailAddressesResponse = res
    "ListVerifiedEmailAddressesResponse"
    "fixture/ListVerifiedEmailAddressesResponse.proto"
    sES
    (Proxy :: Proxy ListVerifiedEmailAddresses)

testSetIdentityNotificationTopicResponse :: SetIdentityNotificationTopicResponse -> TestTree
testSetIdentityNotificationTopicResponse = res
    "SetIdentityNotificationTopicResponse"
    "fixture/SetIdentityNotificationTopicResponse.proto"
    sES
    (Proxy :: Proxy SetIdentityNotificationTopic)

testSendEmailResponse :: SendEmailResponse -> TestTree
testSendEmailResponse = res
    "SendEmailResponse"
    "fixture/SendEmailResponse.proto"
    sES
    (Proxy :: Proxy SendEmail)

testDeleteReceiptRuleResponse :: DeleteReceiptRuleResponse -> TestTree
testDeleteReceiptRuleResponse = res
    "DeleteReceiptRuleResponse"
    "fixture/DeleteReceiptRuleResponse.proto"
    sES
    (Proxy :: Proxy DeleteReceiptRule)

testUpdateReceiptRuleResponse :: UpdateReceiptRuleResponse -> TestTree
testUpdateReceiptRuleResponse = res
    "UpdateReceiptRuleResponse"
    "fixture/UpdateReceiptRuleResponse.proto"
    sES
    (Proxy :: Proxy UpdateReceiptRule)

testCloneReceiptRuleSetResponse :: CloneReceiptRuleSetResponse -> TestTree
testCloneReceiptRuleSetResponse = res
    "CloneReceiptRuleSetResponse"
    "fixture/CloneReceiptRuleSetResponse.proto"
    sES
    (Proxy :: Proxy CloneReceiptRuleSet)

testCreateReceiptRuleResponse :: CreateReceiptRuleResponse -> TestTree
testCreateReceiptRuleResponse = res
    "CreateReceiptRuleResponse"
    "fixture/CreateReceiptRuleResponse.proto"
    sES
    (Proxy :: Proxy CreateReceiptRule)

testSetActiveReceiptRuleSetResponse :: SetActiveReceiptRuleSetResponse -> TestTree
testSetActiveReceiptRuleSetResponse = res
    "SetActiveReceiptRuleSetResponse"
    "fixture/SetActiveReceiptRuleSetResponse.proto"
    sES
    (Proxy :: Proxy SetActiveReceiptRuleSet)

testDescribeActiveReceiptRuleSetResponse :: DescribeActiveReceiptRuleSetResponse -> TestTree
testDescribeActiveReceiptRuleSetResponse = res
    "DescribeActiveReceiptRuleSetResponse"
    "fixture/DescribeActiveReceiptRuleSetResponse.proto"
    sES
    (Proxy :: Proxy DescribeActiveReceiptRuleSet)
