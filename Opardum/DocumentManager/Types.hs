{-# LANGUAGE EmptyDataDecls, TypeFamilies #-}
module Opardum.DocumentManager.Types where
import Opardum.ConcurrencyControl
import Opardum.Websockets
import Opardum.Processes

-- Actually in the Hidden Types module.
data DocumentManager 

data instance ProcessCommands DocumentManager = NewClient Client
                                              | NewOp Client Packet
                                              | RemoveClient Client
