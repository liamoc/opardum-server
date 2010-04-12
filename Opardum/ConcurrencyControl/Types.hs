module Opardum.ConcurrencyControl.Types where

import Opardum.OperationalTransforms
import Opardum.Websockets

type Packet = (Int, Op)

data DocumentManagerMsg = NewClient Client
                        | NewOp Client Packet
                        | RemoveClient Client
                        deriving (Show)

data ClientManagerMsg = AddClient Client
                      | AddClientToDoc Client String
                      | RemoveDocument String
                      deriving (Show)


