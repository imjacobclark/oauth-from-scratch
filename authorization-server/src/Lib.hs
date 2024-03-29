{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( server
    ) where

import Network.HTTP.Types (status302, status404)
import Web.Scotty

import Control.Monad.IO.Class
import Data.Monoid (mconcat)

import qualified Data.Text.Lazy as LT

import Controllers.Authorize
import Utils
import Models.Client
import Cache

import Data.UUID
import Data.UUID.V4 ( nextRandom )

requestParamsToClient :: LT.Text -> LT.Text -> LT.Text -> LT.Text -> Client
requestParamsToClient id secret redirectUri scope = do
        let clientId = stringToInt . lazyTextToString $ id
        let clientSecret = stringToInt . lazyTextToString $ secret
        let clientRedirectUri = lazyTextToString $ redirectUri
        let clientScope = scopeStringToScopeTypeList . lazyTextToString $ scope

        Client (ClientID clientId) clientSecret clientScope [clientRedirectUri]

generateRequestID :: IO String
generateRequestID = do
    let randomUUID = nextRandom
    toString <$> randomUUID

notFoundErrorMessage = "Couldn't find what you were looking for."

server :: IO ()
server = do
    cacheClient <- newClientCache
    scotty 3000 $ do
        get "/" $ do
            html "authorization-server"

        -- localhost:3000/authorize?client_id=1&client_id=123456789&redirect_uri=http://localhost:3000/callback&scope=read
        get "/authorize" $ do
            requestId <- liftIO $ generateRequestID

            clientIdParam <- param "client_id"
            clientSecretParam <- param "client_id"
            redirectUriParam <- param "redirect_uri"
            scopeParam <- param "scope"

            let client = validateClientRequestingAuthorization $ requestParamsToClient clientIdParam clientSecretParam redirectUriParam scopeParam

            case client of
                (Just client) -> do
                    liftIO $ insertInflightClientIntoCache cacheClient client requestId
                    status status302
                    setHeader "X-Forwarded-From" "/authorize"
                    setHeader "Location" $ LT.pack ("/approve?request_id=" ++ requestId)
                Nothing -> do
                    status status404
                    html notFoundErrorMessage

        get "/approve" $ do
            requestId <- param "request_id"

            client <- liftIO $ findInflightClientInCache cacheClient requestId

            case client of
                (Just client) -> do
                    html $ mconcat [LT.pack . show $ client]
                Nothing -> do
                    status status404
                    html notFoundErrorMessage

            