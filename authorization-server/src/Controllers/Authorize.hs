module Controllers.Authorize where
    
import Data.List
import Data.Either

import Models.Client
import Data.Clients

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

validateClientRequestingAuthorization :: Client -> Maybe Client
validateClientRequestingAuthorization client
    | findClient client == Nothing = Nothing
    | validateRequestedScope (findClient client) (scope client) == Nothing = Nothing
    | validateRedirectUri (findClient client) (getFirstRedirectUri . redirectUris $ client) == Nothing = Nothing
    | otherwise = Just client