{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is the /AWS WAF API Reference/. This guide is for developers who
-- need detailed information about the AWS WAF API actions, data types, and
-- errors. For detailed information about AWS WAF features and an overview
-- of how to use the AWS WAF API, see the
-- <http://docs.aws.amazon.com/waf/latest/dev/ AWS WAF Developer Guide>.
--
-- /See:/ <http://docs.aws.amazon.com/waf/latest/APIReference/Welcome.html AWS API Reference>
module Network.AWS.WAF
    (
    -- * Service Configuration
      wAF

    -- * Errors
    -- $errors

    -- ** WAFInvalidAccountException
    , _WAFInvalidAccountException

    -- ** WAFReferencedItemException
    , _WAFReferencedItemException

    -- ** WAFInvalidOperationException
    , _WAFInvalidOperationException

    -- ** WAFNonexistentItemException
    , _WAFNonexistentItemException

    -- ** WAFInvalidParameterException
    , _WAFInvalidParameterException

    -- ** WAFLimitsExceededException
    , _WAFLimitsExceededException

    -- ** WAFStaleDataException
    , _WAFStaleDataException

    -- ** WAFInternalErrorException
    , _WAFInternalErrorException

    -- ** WAFNonexistentContainerException
    , _WAFNonexistentContainerException

    -- ** WAFDisallowedNameException
    , _WAFDisallowedNameException

    -- ** WAFNonEmptyEntityException
    , _WAFNonEmptyEntityException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** UpdateRule
    , module Network.AWS.WAF.UpdateRule

    -- ** DeleteRule
    , module Network.AWS.WAF.DeleteRule

    -- ** CreateIPSet
    , module Network.AWS.WAF.CreateIPSet

    -- ** GetChangeTokenStatus
    , module Network.AWS.WAF.GetChangeTokenStatus

    -- ** DeleteWebACL
    , module Network.AWS.WAF.DeleteWebACL

    -- ** UpdateWebACL
    , module Network.AWS.WAF.UpdateWebACL

    -- ** ListWebACLs
    , module Network.AWS.WAF.ListWebACLs

    -- ** ListRules
    , module Network.AWS.WAF.ListRules

    -- ** CreateRule
    , module Network.AWS.WAF.CreateRule

    -- ** CreateWebACL
    , module Network.AWS.WAF.CreateWebACL

    -- ** ListByteMatchSets
    , module Network.AWS.WAF.ListByteMatchSets

    -- ** GetIPSet
    , module Network.AWS.WAF.GetIPSet

    -- ** GetWebACL
    , module Network.AWS.WAF.GetWebACL

    -- ** GetRule
    , module Network.AWS.WAF.GetRule

    -- ** GetChangeToken
    , module Network.AWS.WAF.GetChangeToken

    -- ** GetSampledRequests
    , module Network.AWS.WAF.GetSampledRequests

    -- ** GetSqlInjectionMatchSet
    , module Network.AWS.WAF.GetSqlInjectionMatchSet

    -- ** CreateSqlInjectionMatchSet
    , module Network.AWS.WAF.CreateSqlInjectionMatchSet

    -- ** CreateByteMatchSet
    , module Network.AWS.WAF.CreateByteMatchSet

    -- ** UpdateByteMatchSet
    , module Network.AWS.WAF.UpdateByteMatchSet

    -- ** DeleteByteMatchSet
    , module Network.AWS.WAF.DeleteByteMatchSet

    -- ** DeleteIPSet
    , module Network.AWS.WAF.DeleteIPSet

    -- ** UpdateIPSet
    , module Network.AWS.WAF.UpdateIPSet

    -- ** ListIPSets
    , module Network.AWS.WAF.ListIPSets

    -- ** GetByteMatchSet
    , module Network.AWS.WAF.GetByteMatchSet

    -- ** ListSqlInjectionMatchSets
    , module Network.AWS.WAF.ListSqlInjectionMatchSets

    -- ** DeleteSqlInjectionMatchSet
    , module Network.AWS.WAF.DeleteSqlInjectionMatchSet

    -- ** UpdateSqlInjectionMatchSet
    , module Network.AWS.WAF.UpdateSqlInjectionMatchSet

    -- * Types

    -- ** ChangeAction
    , ChangeAction (..)

    -- ** ChangeTokenStatus
    , ChangeTokenStatus (..)

    -- ** IPSetDescriptorType
    , IPSetDescriptorType (..)

    -- ** MatchFieldType
    , MatchFieldType (..)

    -- ** PositionalConstraint
    , PositionalConstraint (..)

    -- ** PredicateType
    , PredicateType (..)

    -- ** TextTransformation
    , TextTransformation (..)

    -- ** WafActionType
    , WafActionType (..)

    -- ** ActivatedRule
    , ActivatedRule
    , activatedRule
    , arPriority
    , arRuleId
    , arAction

    -- ** ByteMatchSet
    , ByteMatchSet
    , byteMatchSet
    , bmsName
    , bmsByteMatchSetId
    , bmsByteMatchTuples

    -- ** ByteMatchSetSummary
    , ByteMatchSetSummary
    , byteMatchSetSummary
    , bmssByteMatchSetId
    , bmssName

    -- ** ByteMatchSetUpdate
    , ByteMatchSetUpdate
    , byteMatchSetUpdate
    , bmsuAction
    , bmsuByteMatchTuple

    -- ** ByteMatchTuple
    , ByteMatchTuple
    , byteMatchTuple
    , bmtFieldToMatch
    , bmtTargetString
    , bmtTextTransformation
    , bmtPositionalConstraint

    -- ** FieldToMatch
    , FieldToMatch
    , fieldToMatch
    , ftmData
    , ftmType

    -- ** HTTPHeader
    , HTTPHeader
    , hTTPHeader
    , httphValue
    , httphName

    -- ** HTTPRequest
    , HTTPRequest
    , hTTPRequest
    , httprHTTPVersion
    , httprCountry
    , httprURI
    , httprHeaders
    , httprMethod
    , httprClientIP

    -- ** IPSet
    , IPSet
    , ipSet
    , isName
    , isIPSetId
    , isIPSetDescriptors

    -- ** IPSetDescriptor
    , IPSetDescriptor
    , ipSetDescriptor
    , isdType
    , isdValue

    -- ** IPSetSummary
    , IPSetSummary
    , ipSetSummary
    , issIPSetId
    , issName

    -- ** IPSetUpdate
    , IPSetUpdate
    , ipSetUpdate
    , isuAction
    , isuIPSetDescriptor

    -- ** Predicate
    , Predicate
    , predicate
    , pNegated
    , pType
    , pDataId

    -- ** Rule
    , Rule
    , rule
    , rMetricName
    , rName
    , rRuleId
    , rPredicates

    -- ** RuleSummary
    , RuleSummary
    , ruleSummary
    , rsRuleId
    , rsName

    -- ** RuleUpdate
    , RuleUpdate
    , ruleUpdate
    , ruAction
    , ruPredicate

    -- ** SampledHTTPRequest
    , SampledHTTPRequest
    , sampledHTTPRequest
    , shttprAction
    , shttprTimestamp
    , shttprRequest
    , shttprWeight

    -- ** SqlInjectionMatchSet
    , SqlInjectionMatchSet
    , sqlInjectionMatchSet
    , simsName
    , simsSqlInjectionMatchSetId
    , simsSqlInjectionMatchTuples

    -- ** SqlInjectionMatchSetSummary
    , SqlInjectionMatchSetSummary
    , sqlInjectionMatchSetSummary
    , simssSqlInjectionMatchSetId
    , simssName

    -- ** SqlInjectionMatchSetUpdate
    , SqlInjectionMatchSetUpdate
    , sqlInjectionMatchSetUpdate
    , simsuAction
    , simsuSqlInjectionMatchTuple

    -- ** SqlInjectionMatchTuple
    , SqlInjectionMatchTuple
    , sqlInjectionMatchTuple
    , simtFieldToMatch
    , simtTextTransformation

    -- ** TimeWindow
    , TimeWindow
    , timeWindow
    , twStartTime
    , twEndTime

    -- ** WafAction
    , WafAction
    , wafAction
    , waType

    -- ** WebACL
    , WebACL
    , webACL
    , waMetricName
    , waName
    , waWebACLId
    , waDefaultAction
    , waRules

    -- ** WebACLSummary
    , WebACLSummary
    , webACLSummary
    , wasWebACLId
    , wasName

    -- ** WebACLUpdate
    , WebACLUpdate
    , webACLUpdate
    , wauAction
    , wauActivatedRule
    ) where

import           Network.AWS.WAF.CreateByteMatchSet
import           Network.AWS.WAF.CreateIPSet
import           Network.AWS.WAF.CreateRule
import           Network.AWS.WAF.CreateSqlInjectionMatchSet
import           Network.AWS.WAF.CreateWebACL
import           Network.AWS.WAF.DeleteByteMatchSet
import           Network.AWS.WAF.DeleteIPSet
import           Network.AWS.WAF.DeleteRule
import           Network.AWS.WAF.DeleteSqlInjectionMatchSet
import           Network.AWS.WAF.DeleteWebACL
import           Network.AWS.WAF.GetByteMatchSet
import           Network.AWS.WAF.GetChangeToken
import           Network.AWS.WAF.GetChangeTokenStatus
import           Network.AWS.WAF.GetIPSet
import           Network.AWS.WAF.GetRule
import           Network.AWS.WAF.GetSampledRequests
import           Network.AWS.WAF.GetSqlInjectionMatchSet
import           Network.AWS.WAF.GetWebACL
import           Network.AWS.WAF.ListByteMatchSets
import           Network.AWS.WAF.ListIPSets
import           Network.AWS.WAF.ListRules
import           Network.AWS.WAF.ListSqlInjectionMatchSets
import           Network.AWS.WAF.ListWebACLs
import           Network.AWS.WAF.Types
import           Network.AWS.WAF.UpdateByteMatchSet
import           Network.AWS.WAF.UpdateIPSet
import           Network.AWS.WAF.UpdateRule
import           Network.AWS.WAF.UpdateSqlInjectionMatchSet
import           Network.AWS.WAF.UpdateWebACL
import           Network.AWS.WAF.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'WAF'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
