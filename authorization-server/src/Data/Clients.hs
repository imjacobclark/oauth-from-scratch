module Data.Clients where

import Data.List

import Models.Client

readClient = Client (ClientID 1) 123456789 [Read] ["http://localhost:3000/callback"]
writeClient = Client (ClientID 2) 987654321 [Write] ["http://localhost:3000/callback"]
writeAndReadClient = Client (ClientID 3) 123459876 [Read, Write] ["http://localhost:3000/callback"]
unscopedClient = Client (ClientID 3) 987612345 [] ["http://localhost:3000/callback"]
noRedirectClient = Client (ClientID 4) 1357908642 [Read] []
redirectClientA = Client (ClientID 4) 1357908642 [Read] ["http://localhost:3000/callback"]

getAllClientsInMemory :: [Client]
getAllClientsInMemory = [readClient, writeClient, writeAndReadClient, unscopedClient]

findClientByClientID :: ClientID -> Maybe Client
findClientByClientID clientIDToFind = find (\client -> (clientId $ Models.Client.id client) == (clientId clientIDToFind)) getAllClientsInMemory

findClient :: Client -> Maybe Client
findClient client = findClientByClientID . Models.Client.id $ client