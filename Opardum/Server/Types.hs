{-# LANGUAGE EmptyDataDecls, TypeFamilies #-}
module Opardum.Server.Types where

import Opardum.Websockets
import Opardum.Processes

data instance ProcessCommands ClientManager = AddClient Client
                                            | AddClientToDoc Client String
                                            | RemoveDocument String
                                            deriving (Show)
data ClientManager
