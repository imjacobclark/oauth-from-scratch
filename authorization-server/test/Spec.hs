import Test.HUnit
import Data.List
import Data.Either

data Scope = Read | Write deriving (Show, Eq)

newtype ClientID = ClientID { clientId :: Int } deriving (Show, Eq)

data Client = Client { id :: ClientID
                        , clientSecret :: Int
                        , scope :: [Scope]
                        , redirectUris :: [String]
                    } deriving (Show, Eq)

readClient = Client (ClientID 1) 123456789 [Read] ["http://localhost:3000/callback"]
writeClient = Client (ClientID 2) 987654321 [Write] ["http://localhost:3000/callback"]
writeAndReadClient = Client (ClientID 3) 123459876 [Read, Write] ["http://localhost:3000/callback"]
unscopedClient = Client (ClientID 3) 987612345 [] ["http://localhost:3000/callback"]

getAllClients :: [Client]
getAllClients = [readClient, writeClient]

findClientByClientID :: ClientID -> Maybe Client
findClientByClientID clientIDToFind = find (\client -> (clientId $ Main.id client) == (clientId clientIDToFind)) getAllClients

validateClientHasScope :: Client -> Scope -> Maybe Scope
validateClientHasScope client requestedScope = find (\clientScope -> clientScope == requestedScope) (scope client)

validateRequestedScope :: Client -> Maybe [Scope] -> Maybe [Scope]
validateRequestedScope _ Nothing = Nothing
validateRequestedScope client (Just requestedScopes) =
    let scopeValidationResults = validateClientHasScope client <$> requestedScopes
     in sequence scopeValidationResults

-- 

testWithUnknownClientId             = TestCase (assertEqual "returns Nothing when an unknown clientID is requested" Nothing (findClientByClientID $ ClientID 0))
testWithKnownReadClientId           = TestCase (assertEqual "returns Just Client when a known clientID is requested" (Just $ readClient) (findClientByClientID $ ClientID 1))
testWithKnownWriteClientId          = TestCase (assertEqual "returns Just Client when a known clientID is requested" (Just $ writeClient) (findClientByClientID $ ClientID 2))
testWithKnownReadAndWriteClientId   = TestCase (assertEqual "returns Just Client when a known clientID is requested" (Just $ writeClient) (findClientByClientID $ ClientID 2))

testWithUnknownScope                                        = TestCase (assertEqual "returns Nothing when no scope is requested" Nothing (validateRequestedScope unscopedClient Nothing))
testWithValidRequestedReadScope_AgainstAReadClient          = TestCase (assertEqual "returns Just Read when a read scope is requested against a read client" (Just [Read]) (validateRequestedScope readClient (Just [Read])))
testWithInvalidRequestedReadScope_AgainstAWriteClient       = TestCase (assertEqual "returns Nothing when a write scope is requested against a read client" Nothing (validateRequestedScope readClient (Just [Write])))
testWithARequestedReadWriteScope_AgainstAReadOnlyClient     = TestCase (assertEqual "returns Nothing when a read/write scope is requested against a read only client" Nothing (validateRequestedScope readClient (Just [Read, Write])))
testWithARequestedReadWriteScope_AgainstAWriteOnlyClient    = TestCase (assertEqual "returns Nothing when a read/write scope is requested against a write only client" Nothing (validateRequestedScope writeClient (Just [Read, Write])))
testWithARequestedReadWriteScope_AgainstAnUnscopedClient    = TestCase (assertEqual "returns Nothing when a read/write scope is requested against an unscoped client" Nothing (validateRequestedScope unscopedClient (Just [Read, Write])))
testWithARequestedReadWriteScope_AgainstAReadWriteClient    = TestCase (assertEqual "returns Just [Just Read, Just Write] when a read/write scope is requested against an unscoped client" (Just [Read, Write]) (validateRequestedScope writeAndReadClient (Just [Read, Write])))

tests = TestList [
    TestLabel "findClientByClientID" testWithUnknownClientId,
    TestLabel "findClientByClientID" testWithKnownReadClientId,
    TestLabel "findClientByClientID" testWithKnownWriteClientId,
    TestLabel "findClientByClientID" testWithKnownReadAndWriteClientId,
    TestLabel "validateRequestedScope" testWithValidRequestedReadScope_AgainstAReadClient,
    TestLabel "validateRequestedScope" testWithInvalidRequestedReadScope_AgainstAWriteClient,
    TestLabel "validateRequestedScope" testWithARequestedReadWriteScope_AgainstAReadOnlyClient,
    TestLabel "validateRequestedScope" testWithARequestedReadWriteScope_AgainstAWriteOnlyClient,
    TestLabel "validateRequestedScope" testWithARequestedReadWriteScope_AgainstAnUnscopedClient,
    TestLabel "validateRequestedScope" testWithARequestedReadWriteScope_AgainstAReadWriteClient
    ]

main :: IO Counts
main = runTestTT tests