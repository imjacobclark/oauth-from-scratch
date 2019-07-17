module Controllers.AuthorizeSpec where

import Test.HUnit

import Controllers.Authorize
import Models.Client
import Data.Clients

testWithUnknownScope                                        = TestCase (assertEqual "returns Nothing when no scope is requested" Nothing (validateRequestedScope (Just unscopedClient) []))
testWithValidRequestedReadScope_AgainstAReadClient          = TestCase (assertEqual "returns Just Read when a read scope is requested against a read client" (Just [Read]) (validateRequestedScope (Just readClient) [Read]))
testWithInvalidRequestedReadScope_AgainstAWriteClient       = TestCase (assertEqual "returns Nothing when a write scope is requested against a read client" Nothing (validateRequestedScope (Just readClient) [Write]))
testWithARequestedReadWriteScope_AgainstAReadOnlyClient     = TestCase (assertEqual "returns Nothing when a read/write scope is requested against a read only client" Nothing (validateRequestedScope (Just readClient) [Read, Write]))
testWithARequestedReadWriteScope_AgainstAWriteOnlyClient    = TestCase (assertEqual "returns Nothing when a read/write scope is requested against a write only client" Nothing (validateRequestedScope (Just writeClient) [Read, Write]))
testWithARequestedReadWriteScope_AgainstAnUnscopedClient    = TestCase (assertEqual "returns Nothing when a read/write scope is requested against an unscoped client" Nothing (validateRequestedScope (Just unscopedClient) [Read, Write]))
testWithARequestedReadWriteScope_AgainstAReadWriteClient    = TestCase (assertEqual "returns Just [Just Read, Just Write] when a read/write scope is requested against an unscoped client" (Just [Read, Write]) (validateRequestedScope (Just writeAndReadClient) [Read, Write]))

testWithEmptyStringRedirectUri      = TestCase (assertEqual "returns Nothing when an empty redirect uri string is requested" Nothing (validateRedirectUri (Just noRedirectClient) ""))
testWithUnknownRedirectUri          = TestCase (assertEqual "returns Nothing when unknown redirect uri is requested" Nothing (validateRedirectUri (Just redirectClientA) "http://google.com:3000/callback"))
testWithKnownRedirectUri            = TestCase (assertEqual "returns Just \"http://localhost:3000/callback\" when a known redirect uri is requested" (Just "http://localhost:3000/callback") (validateRedirectUri (Just redirectClientA) "http://localhost:3000/callback"))

testWithValidClient                         = TestCase (assertEqual "returns Just Client when a client with matching properties is requested" (Just $ Client (ClientID 1) 123456789 [Read] ["http://localhost:3000/callback"]) (validateClientRequestingAuthorization $ Client (ClientID 1) 123456789 [Read] ["http://localhost:3000/callback"]))
testWithUnknownClient                       = TestCase (assertEqual "returns Nothing when an unknown client is requested" Nothing (validateClientRequestingAuthorization $ Client (ClientID 10000) 123456789 [Read] ["http://localhost:3000/callback"]))
testWithKnownClientButInvalidScope          = TestCase (assertEqual "returns Nothing when a known client with an invalid scope is requested" Nothing (validateClientRequestingAuthorization $ Client (ClientID 1) 123456789 [Write] ["http://localhost:3000/callback"]))
testWithKnownClientButInvalidRedirectURI    = TestCase (assertEqual "returns Nothing when a known client with an invalid redirect URI is requested" Nothing (validateClientRequestingAuthorization $ Client (ClientID 1) 123456789 [Read] ["http://google.com:3000/callback"]))

testWithSingleElement = TestCase (assertEqual "returns first string when string is specified" "first" (getFirstRedirectUri ["first"]))
testWithTwoElements = TestCase (assertEqual "returns first string when two strings are specified" "first" (getFirstRedirectUri ["first", "second"]))
testWithZeroElements  = TestCase (assertEqual "returns empty string when no strings are specified" "" (getFirstRedirectUri []))

authorizeSpecTests = [
    TestLabel "validateRequestedScope" testWithValidRequestedReadScope_AgainstAReadClient,
    TestLabel "validateRequestedScope" testWithInvalidRequestedReadScope_AgainstAWriteClient,
    TestLabel "validateRequestedScope" testWithARequestedReadWriteScope_AgainstAReadOnlyClient,
    TestLabel "validateRequestedScope" testWithARequestedReadWriteScope_AgainstAWriteOnlyClient,
    TestLabel "validateRequestedScope" testWithARequestedReadWriteScope_AgainstAnUnscopedClient,
    TestLabel "validateRequestedScope" testWithARequestedReadWriteScope_AgainstAReadWriteClient,
    TestLabel "testWithEmptyStringRedirectUri" testWithEmptyStringRedirectUri,
    TestLabel "testWithUnknownRedirectUri" testWithUnknownRedirectUri,
    TestLabel "testWithKnownRedirectUri" testWithKnownRedirectUri,
    TestLabel "testWithValidClient" testWithValidClient,
    TestLabel "testWithUnknownClient" testWithUnknownClient,
    TestLabel "testWithKnownClientButInvalidScope" testWithKnownClientButInvalidScope,
    TestLabel "testWithKnownClientButInvalidRedirectURI" testWithKnownClientButInvalidRedirectURI,
    TestLabel "testWithSingleElement" testWithSingleElement,
    TestLabel "testWithTwoElements" testWithTwoElements,
    TestLabel "testWithZeroElements" testWithZeroElements

    ]