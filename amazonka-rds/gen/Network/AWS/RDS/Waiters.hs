{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Waiters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Waiters where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.RDS.DescribeDBInstances
import           Network.AWS.RDS.DescribeDBInstances
import           Network.AWS.RDS.DescribeDBSnapshots
import           Network.AWS.RDS.Types
import           Network.AWS.Waiter

-- | Polls 'Network.AWS.RDS.DescribeDBInstances' every 30 seconds until a
-- successful state is reached. An error is returned after 60 failed checks.
dbInstanceAvailable :: Wait DescribeDBInstances
dbInstanceAvailable =
    Wait
    { _waitName = "DBInstanceAvailable"
    , _waitAttempts = 60
    , _waitDelay = 30
    , _waitAcceptors = [ matchAll
                             "available"
                             AcceptSuccess
                             (folding (concatOf ddbirsDBInstances) .
                              diDBInstanceStatus . _Just . to toTextCI)
                       , matchAny
                             "deleted"
                             AcceptFailure
                             (folding (concatOf ddbirsDBInstances) .
                              diDBInstanceStatus . _Just . to toTextCI)
                       , matchAny
                             "deleting"
                             AcceptFailure
                             (folding (concatOf ddbirsDBInstances) .
                              diDBInstanceStatus . _Just . to toTextCI)
                       , matchAny
                             "failed"
                             AcceptFailure
                             (folding (concatOf ddbirsDBInstances) .
                              diDBInstanceStatus . _Just . to toTextCI)
                       , matchAny
                             "incompatible-restore"
                             AcceptFailure
                             (folding (concatOf ddbirsDBInstances) .
                              diDBInstanceStatus . _Just . to toTextCI)
                       , matchAny
                             "incompatible-parameters"
                             AcceptFailure
                             (folding (concatOf ddbirsDBInstances) .
                              diDBInstanceStatus . _Just . to toTextCI)
                       , matchAny
                             "incompatible-parameters"
                             AcceptFailure
                             (folding (concatOf ddbirsDBInstances) .
                              diDBInstanceStatus . _Just . to toTextCI)
                       , matchAny
                             "incompatible-restore"
                             AcceptFailure
                             (folding (concatOf ddbirsDBInstances) .
                              diDBInstanceStatus . _Just . to toTextCI)]
    }

-- | Polls 'Network.AWS.RDS.DescribeDBSnapshots' every 15 seconds until a
-- successful state is reached. An error is returned after 40 failed checks.
dbSnapshotCompleted :: Wait DescribeDBSnapshots
dbSnapshotCompleted =
    Wait
    { _waitName = "DBSnapshotCompleted"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors = [ matchError "DBSnapshotNotFound" AcceptSuccess
                       , matchAll
                             "available"
                             AcceptSuccess
                             (folding (concatOf ddsrsDBSnapshots) .
                              dsStatus . _Just . to toTextCI)]
    }

-- | Polls 'Network.AWS.RDS.DescribeDBInstances' every 30 seconds until a
-- successful state is reached. An error is returned after 60 failed checks.
dbInstanceDeleted :: Wait DescribeDBInstances
dbInstanceDeleted =
    Wait
    { _waitName = "DBInstanceDeleted"
    , _waitAttempts = 60
    , _waitDelay = 30
    , _waitAcceptors = [ matchError "DBInstanceNotFound" AcceptSuccess
                       , matchAll
                             "deleted"
                             AcceptSuccess
                             (folding (concatOf ddbirsDBInstances) .
                              diDBInstanceStatus . _Just . to toTextCI)
                       , matchAny
                             "creating"
                             AcceptFailure
                             (folding (concatOf ddbirsDBInstances) .
                              diDBInstanceStatus . _Just . to toTextCI)
                       , matchAny
                             "modifying"
                             AcceptFailure
                             (folding (concatOf ddbirsDBInstances) .
                              diDBInstanceStatus . _Just . to toTextCI)
                       , matchAny
                             "rebooting"
                             AcceptFailure
                             (folding (concatOf ddbirsDBInstances) .
                              diDBInstanceStatus . _Just . to toTextCI)
                       , matchAny
                             "resetting-master-credentials"
                             AcceptFailure
                             (folding (concatOf ddbirsDBInstances) .
                              diDBInstanceStatus . _Just . to toTextCI)]
    }
