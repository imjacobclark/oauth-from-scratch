module Controllers.Authorize where
    
import Data.List
import Data.Either

import Models.Client

readClient = Client (ClientID 1) 123456789 [Read] ["http://localhost:3000/callback"]
writeClient = Client (ClientID 2) 987654321 [Write] ["http://localhost:3000/callback"]
writeAndReadClient = Client (ClientID 3) 123459876 [Read, Write] ["http://localhost:3000/callback"]
unscopedClient = Client (ClientID 3) 987612345 [] ["http://localhost:3000/callback"]
noRedirectClient = Client (ClientID 4) 1357908642 [Read] []
redirectClientA = Client (ClientID 4) 1357908642 [Read] ["http://localhost:3000/callback"]

validateClientHasScope :: Client -> Scope -> Maybe Scope
validateClientHasScope client requestedScope = find (== requestedScope) (scope client)

validateRequestedScope :: Maybe Client -> [Scope] -> Maybe [Scope]
validateRequestedScope Nothing [] = Nothing
validateRequestedScope (Just client) requestedScopes =
    let scopeValidationResults = validateClientHasScope client <$> requestedScopes
     in sequence scopeValidationResults

validateRedirectUri :: Maybe Client -> String-> Maybe String
validateRedirectUri Nothing _ = Nothing
validateRedirectUri _ "" = Nothing
validateRedirectUri (Just client) redirectUri = find (== redirectUri) (redirectUris client)

getFirstRedirectUri :: [String] -> String
getFirstRedirectUri [] = ""
getFirstRedirectUri (x:_) = x

getAllClients :: [Client]
getAllClients = [readClient, writeClient, writeAndReadClient, unscopedClient]

findClientByClientID :: ClientID -> Maybe Client
findClientByClientID clientIDToFind = find (\client -> (clientId $ Models.Client.id client) == (clientId clientIDToFind)) getAllClients

findClient :: Client -> Maybe Client
findClient client = findClientByClientID . Models.Client.id $ client

validateClientRequestingAuthorization :: Client -> Maybe Client
validateClientRequestingAuthorization client
    | findClient client == Nothing = Nothing
    | validateRequestedScope (findClient client) (scope client) == Nothing = Nothing
    | validateRedirectUri (findClient client) (getFirstRedirectUri . redirectUris $ client) == Nothing = Nothing
    | otherwise = Just client