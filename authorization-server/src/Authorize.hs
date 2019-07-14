module Authorize where
    
import Data.List
import Data.Either

data Scope = Read | Write deriving (Show, Eq)

newtype ClientID = ClientID { 
    clientId :: Int 
} deriving (Show, Eq)

data Client = Client { id :: ClientID
                        , clientSecret :: Int
                        , scope :: [Scope]
                        , redirectUris :: [String]
                    } deriving (Show, Eq)

readClient = Client (ClientID 1) 123456789 [Read] ["http://localhost:3000/callback"]
writeClient = Client (ClientID 2) 987654321 [Write] ["http://localhost:3000/callback"]
writeAndReadClient = Client (ClientID 3) 123459876 [Read, Write] ["http://localhost:3000/callback"]
unscopedClient = Client (ClientID 3) 987612345 [] ["http://localhost:3000/callback"]
noRedirectClient = Client (ClientID 4) 1357908642 [Read] []
redirectClientA = Client (ClientID 4) 1357908642 [Read] ["http://localhost:3000/callback"]

getAllClients :: [Client]
getAllClients = [readClient, writeClient, writeAndReadClient, unscopedClient]

findClientByClientID :: ClientID -> Maybe Client
findClientByClientID clientIDToFind = find (\client -> (clientId $ Authorize.id client) == (clientId clientIDToFind)) getAllClients

validateClientHasScope :: Client -> Scope -> Maybe Scope
validateClientHasScope client requestedScope = find (== requestedScope) (scope client)

validateRequestedScope :: Client -> [Scope] -> Maybe [Scope]
validateRequestedScope _ [] = Nothing
validateRequestedScope client requestedScopes =
    let scopeValidationResults = validateClientHasScope client <$> requestedScopes
     in sequence scopeValidationResults

validateRedirectUri :: Client -> String-> Maybe String
validateRedirectUri client [] = Nothing
validateRedirectUri client redirectUri = find (== redirectUri) (redirectUris client)