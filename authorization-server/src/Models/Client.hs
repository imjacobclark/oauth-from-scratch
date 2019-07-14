module Models.Client where

import Data.List.Split

data Scope = Read | Write deriving (Show, Eq)

newtype ClientID = ClientID { 
    clientId :: Int 
} deriving (Show, Eq)

data Client = Client { id :: ClientID
                        , clientSecret :: Int
                        , scope :: [Scope]
                        , redirectUris :: [String]
                    } deriving (Show, Eq)

scopeStringToScopeType :: String -> Scope
scopeStringToScopeType "read" = Read
scopeStringToScopeType "write" = Write

scopeStringToScopeTypeList :: String -> [Scope]
scopeStringToScopeTypeList "" = []
scopeStringToScopeTypeList x = fmap scopeStringToScopeType (splitOn " " x)