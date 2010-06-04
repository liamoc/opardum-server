module Opardum.Server.Types where

import Opardum.Websockets

data ClientManagerMsg = AddClient Client
                      | AddClientToDoc Client String
                      | RemoveDocument String
                      deriving (Show)
data ClientManager = ClientManager
