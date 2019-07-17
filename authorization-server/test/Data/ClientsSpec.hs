module Data.ClientsSpec where

import Test.HUnit

import Models.Client
import Data.Clients

testWithUnknownClientId             = TestCase (assertEqual "returns Nothing when an unknown clientID is requested" Nothing (findClientByClientID $ ClientID 0))
testWithKnownReadClientId           = TestCase (assertEqual "returns Just Client when a known clientID is requested" (Just $ readClient) (findClientByClientID $ ClientID 1))
testWithKnownWriteClientId          = TestCase (assertEqual "returns Just Client when a known clientID is requested" (Just $ writeClient) (findClientByClientID $ ClientID 2))
testWithKnownReadAndWriteClientId   = TestCase (assertEqual "returns Just Client when a known clientID is requested" (Just $ writeClient) (findClientByClientID $ ClientID 2))

clientsSpecTests = [
    TestLabel "findClientByClientID" testWithUnknownClientId,
    TestLabel "findClientByClientID" testWithKnownReadClientId,
    TestLabel "findClientByClientID" testWithKnownWriteClientId,
    TestLabel "findClientByClientID" testWithKnownReadAndWriteClientId
    ]