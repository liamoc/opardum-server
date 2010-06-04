 module Opardum.DocumentManager.Types where
 import Opardum.ConcurrencyControl
 import Opardum.Websockets

 -- Actually in the Hidden Types module.
 data DocumentManager = DocumentManager


 data DocumentManagerMsg = NewClient Client
                         | NewOp Client Packet
                         | RemoveClient Client
