{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( server
    ) where

import Web.Scotty

import Data.Monoid (mconcat)

server :: IO ()
server = scotty 3000 $
    get "/" $ do
        html "authorization-server"
