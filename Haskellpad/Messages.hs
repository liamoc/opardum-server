module Haskellpad.Messages where

import Haskellpad.OperationalTransforms
import Haskellpad.Websockets

type Packet = (Int, Op)

data DocumentManagerMsg = NewClient Client
                        | NewOp Client Packet
                        | RemoveClient Client
                        deriving (Show)

data ClientManagerMsg = AddClient Client
                      | RemoveDocument String
                      deriving (Show)


