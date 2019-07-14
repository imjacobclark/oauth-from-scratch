module AuthorizeSpec where

import Test.HUnit

import Authorize

testWithUnknownClientId             = TestCase (assertEqual "returns Nothing when an unknown clientID is requested" Nothing (findClientByClientID $ ClientID 0))
testWithKnownReadClientId           = TestCase (assertEqual "returns Just Client when a known clientID is requested" (Just $ readClient) (findClientByClientID $ ClientID 1))
testWithKnownWriteClientId          = TestCase (assertEqual "returns Just Client when a known clientID is requested" (Just $ writeClient) (findClientByClientID $ ClientID 2))
testWithKnownReadAndWriteClientId   = TestCase (assertEqual "returns Just Client when a known clientID is requested" (Just $ writeClient) (findClientByClientID $ ClientID 2))

testWithUnknownScope                                        = TestCase (assertEqual "returns Nothing when no scope is requested" Nothing (validateRequestedScope unscopedClient []))
testWithValidRequestedReadScope_AgainstAReadClient          = TestCase (assertEqual "returns Just Read when a read scope is requested against a read client" (Just [Read]) (validateRequestedScope readClient [Read]))
testWithInvalidRequestedReadScope_AgainstAWriteClient       = TestCase (assertEqual "returns Nothing when a write scope is requested against a read client" Nothing (validateRequestedScope readClient [Write]))
testWithARequestedReadWriteScope_AgainstAReadOnlyClient     = TestCase (assertEqual "returns Nothing when a read/write scope is requested against a read only client" Nothing (validateRequestedScope readClient [Read, Write]))
testWithARequestedReadWriteScope_AgainstAWriteOnlyClient    = TestCase (assertEqual "returns Nothing when a read/write scope is requested against a write only client" Nothing (validateRequestedScope writeClient [Read, Write]))
testWithARequestedReadWriteScope_AgainstAnUnscopedClient    = TestCase (assertEqual "returns Nothing when a read/write scope is requested against an unscoped client" Nothing (validateRequestedScope unscopedClient [Read, Write]))
testWithARequestedReadWriteScope_AgainstAReadWriteClient    = TestCase (assertEqual "returns Just [Just Read, Just Write] when a read/write scope is requested against an unscoped client" (Just [Read, Write]) (validateRequestedScope writeAndReadClient [Read, Write]))

testWithEmptyRedirectUri = TestCase (assertEqual "returns Nothing when no redirect uri is requested" Nothing (validateRedirectUri noRedirectClient []))
testWithUnknownRedirectUri = TestCase (assertEqual "returns Nothing when unknown redirect uri is requested" Nothing (validateRedirectUri redirectClientA "http://google.com:3000/callback"))
testWithKnownRedirectUri = TestCase (assertEqual "returns Just \"http://localhost:3000/callback\" when a known redirect uri is requested" (Just "http://localhost:3000/callback") (validateRedirectUri redirectClientA "http://localhost:3000/callback"))

authorizeSpecTests = [
    TestLabel "findClientByClientID" testWithUnknownClientId,
    TestLabel "findClientByClientID" testWithKnownReadClientId,
    TestLabel "findClientByClientID" testWithKnownWriteClientId,
    TestLabel "findClientByClientID" testWithKnownReadAndWriteClientId,
    TestLabel "validateRequestedScope" testWithValidRequestedReadScope_AgainstAReadClient,
    TestLabel "validateRequestedScope" testWithInvalidRequestedReadScope_AgainstAWriteClient,
    TestLabel "validateRequestedScope" testWithARequestedReadWriteScope_AgainstAReadOnlyClient,
    TestLabel "validateRequestedScope" testWithARequestedReadWriteScope_AgainstAWriteOnlyClient,
    TestLabel "validateRequestedScope" testWithARequestedReadWriteScope_AgainstAnUnscopedClient,
    TestLabel "validateRequestedScope" testWithARequestedReadWriteScope_AgainstAReadWriteClient,
    TestLabel "testWithEmptyRedirectUri" testWithEmptyRedirectUri,
    TestLabel "testWithUnknownRedirectUri" testWithUnknownRedirectUri,
    TestLabel "testWithKnownRedirectUri" testWithKnownRedirectUri
    ]