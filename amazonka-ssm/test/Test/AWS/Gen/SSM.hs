{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SSM
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.SSM where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.SSM
import Test.AWS.SSM.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testDescribeDocument $
--             describeDocument
--
--         , testCreateAssociation $
--             createAssociation
--
--         , testCreateDocument $
--             createDocument
--
--         , testListCommandInvocations $
--             listCommandInvocations
--
--         , testListDocuments $
--             listDocuments
--
--         , testGetDocument $
--             getDocument
--
--         , testCancelCommand $
--             cancelCommand
--
--         , testDescribeAssociation $
--             describeAssociation
--
--         , testUpdateAssociationStatus $
--             updateAssociationStatus
--
--         , testDescribeInstanceInformation $
--             describeInstanceInformation
--
--         , testListAssociations $
--             listAssociations
--
--         , testDeleteAssociation $
--             deleteAssociation
--
--         , testSendCommand $
--             sendCommand
--
--         , testListCommands $
--             listCommands
--
--         , testDeleteDocument $
--             deleteDocument
--
--         , testCreateAssociationBatch $
--             createAssociationBatch
--
--           ]

--     , testGroup "response"
--         [ testDescribeDocumentResponse $
--             describeDocumentResponse
--
--         , testCreateAssociationResponse $
--             createAssociationResponse
--
--         , testCreateDocumentResponse $
--             createDocumentResponse
--
--         , testListCommandInvocationsResponse $
--             listCommandInvocationsResponse
--
--         , testListDocumentsResponse $
--             listDocumentsResponse
--
--         , testGetDocumentResponse $
--             getDocumentResponse
--
--         , testCancelCommandResponse $
--             cancelCommandResponse
--
--         , testDescribeAssociationResponse $
--             describeAssociationResponse
--
--         , testUpdateAssociationStatusResponse $
--             updateAssociationStatusResponse
--
--         , testDescribeInstanceInformationResponse $
--             describeInstanceInformationResponse
--
--         , testListAssociationsResponse $
--             listAssociationsResponse
--
--         , testDeleteAssociationResponse $
--             deleteAssociationResponse
--
--         , testSendCommandResponse $
--             sendCommandResponse
--
--         , testListCommandsResponse $
--             listCommandsResponse
--
--         , testDeleteDocumentResponse $
--             deleteDocumentResponse
--
--         , testCreateAssociationBatchResponse $
--             createAssociationBatchResponse
--
--           ]
--     ]

-- Requests

testDescribeDocument :: DescribeDocument -> TestTree
testDescribeDocument = req
    "DescribeDocument"
    "fixture/DescribeDocument.yaml"

testCreateAssociation :: CreateAssociation -> TestTree
testCreateAssociation = req
    "CreateAssociation"
    "fixture/CreateAssociation.yaml"

testCreateDocument :: CreateDocument -> TestTree
testCreateDocument = req
    "CreateDocument"
    "fixture/CreateDocument.yaml"

testListCommandInvocations :: ListCommandInvocations -> TestTree
testListCommandInvocations = req
    "ListCommandInvocations"
    "fixture/ListCommandInvocations.yaml"

testListDocuments :: ListDocuments -> TestTree
testListDocuments = req
    "ListDocuments"
    "fixture/ListDocuments.yaml"

testGetDocument :: GetDocument -> TestTree
testGetDocument = req
    "GetDocument"
    "fixture/GetDocument.yaml"

testCancelCommand :: CancelCommand -> TestTree
testCancelCommand = req
    "CancelCommand"
    "fixture/CancelCommand.yaml"

testDescribeAssociation :: DescribeAssociation -> TestTree
testDescribeAssociation = req
    "DescribeAssociation"
    "fixture/DescribeAssociation.yaml"

testUpdateAssociationStatus :: UpdateAssociationStatus -> TestTree
testUpdateAssociationStatus = req
    "UpdateAssociationStatus"
    "fixture/UpdateAssociationStatus.yaml"

testDescribeInstanceInformation :: DescribeInstanceInformation -> TestTree
testDescribeInstanceInformation = req
    "DescribeInstanceInformation"
    "fixture/DescribeInstanceInformation.yaml"

testListAssociations :: ListAssociations -> TestTree
testListAssociations = req
    "ListAssociations"
    "fixture/ListAssociations.yaml"

testDeleteAssociation :: DeleteAssociation -> TestTree
testDeleteAssociation = req
    "DeleteAssociation"
    "fixture/DeleteAssociation.yaml"

testSendCommand :: SendCommand -> TestTree
testSendCommand = req
    "SendCommand"
    "fixture/SendCommand.yaml"

testListCommands :: ListCommands -> TestTree
testListCommands = req
    "ListCommands"
    "fixture/ListCommands.yaml"

testDeleteDocument :: DeleteDocument -> TestTree
testDeleteDocument = req
    "DeleteDocument"
    "fixture/DeleteDocument.yaml"

testCreateAssociationBatch :: CreateAssociationBatch -> TestTree
testCreateAssociationBatch = req
    "CreateAssociationBatch"
    "fixture/CreateAssociationBatch.yaml"

-- Responses

testDescribeDocumentResponse :: DescribeDocumentResponse -> TestTree
testDescribeDocumentResponse = res
    "DescribeDocumentResponse"
    "fixture/DescribeDocumentResponse.proto"
    sSM
    (Proxy :: Proxy DescribeDocument)

testCreateAssociationResponse :: CreateAssociationResponse -> TestTree
testCreateAssociationResponse = res
    "CreateAssociationResponse"
    "fixture/CreateAssociationResponse.proto"
    sSM
    (Proxy :: Proxy CreateAssociation)

testCreateDocumentResponse :: CreateDocumentResponse -> TestTree
testCreateDocumentResponse = res
    "CreateDocumentResponse"
    "fixture/CreateDocumentResponse.proto"
    sSM
    (Proxy :: Proxy CreateDocument)

testListCommandInvocationsResponse :: ListCommandInvocationsResponse -> TestTree
testListCommandInvocationsResponse = res
    "ListCommandInvocationsResponse"
    "fixture/ListCommandInvocationsResponse.proto"
    sSM
    (Proxy :: Proxy ListCommandInvocations)

testListDocumentsResponse :: ListDocumentsResponse -> TestTree
testListDocumentsResponse = res
    "ListDocumentsResponse"
    "fixture/ListDocumentsResponse.proto"
    sSM
    (Proxy :: Proxy ListDocuments)

testGetDocumentResponse :: GetDocumentResponse -> TestTree
testGetDocumentResponse = res
    "GetDocumentResponse"
    "fixture/GetDocumentResponse.proto"
    sSM
    (Proxy :: Proxy GetDocument)

testCancelCommandResponse :: CancelCommandResponse -> TestTree
testCancelCommandResponse = res
    "CancelCommandResponse"
    "fixture/CancelCommandResponse.proto"
    sSM
    (Proxy :: Proxy CancelCommand)

testDescribeAssociationResponse :: DescribeAssociationResponse -> TestTree
testDescribeAssociationResponse = res
    "DescribeAssociationResponse"
    "fixture/DescribeAssociationResponse.proto"
    sSM
    (Proxy :: Proxy DescribeAssociation)

testUpdateAssociationStatusResponse :: UpdateAssociationStatusResponse -> TestTree
testUpdateAssociationStatusResponse = res
    "UpdateAssociationStatusResponse"
    "fixture/UpdateAssociationStatusResponse.proto"
    sSM
    (Proxy :: Proxy UpdateAssociationStatus)

testDescribeInstanceInformationResponse :: DescribeInstanceInformationResponse -> TestTree
testDescribeInstanceInformationResponse = res
    "DescribeInstanceInformationResponse"
    "fixture/DescribeInstanceInformationResponse.proto"
    sSM
    (Proxy :: Proxy DescribeInstanceInformation)

testListAssociationsResponse :: ListAssociationsResponse -> TestTree
testListAssociationsResponse = res
    "ListAssociationsResponse"
    "fixture/ListAssociationsResponse.proto"
    sSM
    (Proxy :: Proxy ListAssociations)

testDeleteAssociationResponse :: DeleteAssociationResponse -> TestTree
testDeleteAssociationResponse = res
    "DeleteAssociationResponse"
    "fixture/DeleteAssociationResponse.proto"
    sSM
    (Proxy :: Proxy DeleteAssociation)

testSendCommandResponse :: SendCommandResponse -> TestTree
testSendCommandResponse = res
    "SendCommandResponse"
    "fixture/SendCommandResponse.proto"
    sSM
    (Proxy :: Proxy SendCommand)

testListCommandsResponse :: ListCommandsResponse -> TestTree
testListCommandsResponse = res
    "ListCommandsResponse"
    "fixture/ListCommandsResponse.proto"
    sSM
    (Proxy :: Proxy ListCommands)

testDeleteDocumentResponse :: DeleteDocumentResponse -> TestTree
testDeleteDocumentResponse = res
    "DeleteDocumentResponse"
    "fixture/DeleteDocumentResponse.proto"
    sSM
    (Proxy :: Proxy DeleteDocument)

testCreateAssociationBatchResponse :: CreateAssociationBatchResponse -> TestTree
testCreateAssociationBatchResponse = res
    "CreateAssociationBatchResponse"
    "fixture/CreateAssociationBatchResponse.proto"
    sSM
    (Proxy :: Proxy CreateAssociationBatch)
