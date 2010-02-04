-- Haskellpad
-- A collaborative code editor for students and haskellers
-- Inspired by the design of Google Wave's operational transform semantics.
-- Written by Liam O'Connor-Davis, with help from the rest of the Google Wave Team.
-- Released under the BSD3 License.

module Haskellpad.Server where

import Haskellpad.OperationalTransforms
import Haskellpad.ConcurrencyControl
import Haskellpad.Websockets
import Haskellpad.Transport
import Network 
import System.IO
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Text.JSON
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M

-- Naming Convention
-- shouter  - thread named "shouter"
-- shouter' - channel that heads to a shouter's inbox.
-- shouter' - root broadcasting channel to all shouters.


data OperatorMessage = Entry Client
                     | Exit Client
                     | TerminateOperator 
                     deriving (Show, Eq)

data SnapshotterMessage = NewOp Op
                        | SendSnapshot Client 
                        | TerminateSnapshotter
                        deriving (Show, Eq)

(~>) = flip writeChan

(<~) :: Chan a -> a -> IO ()
(<~) = writeChan

debug :: String -> IO ()
debug = putStrLn

------------------------
-- Per Socket Threads -- 
------------------------

packetsmasher inbox snapshotter' = do 
                             msg <- readChan inbox 
                             case msg of
                                Just v -> do case (decode v) :: Result Op of
                                                Error e   -> debug $ "malformed packet: " ++ e ++ " (message was " ++ v ++ ")"
                                                Ok op -> NewOp op ~> snapshotter'
                                             continue
                                Nothing -> debug "packetsmasher terminated."
                   where continue = packetsmasher inbox snapshotter'

listener h packetsmasher' shouter' = do 
                              v <- readFrame h
                              case v of
                                Right str  -> handle str >> continue    
                                Left str   -> handle str >> terminate
                   where terminate  = do Nothing ~> packetsmasher'
                                         Nothing ~> shouter'
                                         debug $ "Listener on " ++ show h ++ " terminated."
                         continue   = listener h packetsmasher' shouter'
                         handle str = when (not $ boring str) $ Just str ~> packetsmasher'
                         boring v   = any ($v) [all (==' '), ((/='[') . head)]

shouter h inbox = do v <- readChan inbox
                     case v of 
                       Nothing  -> terminate
                       Just msg -> do v <- sendFrame h msg
                                      if v then continue else terminate
               where terminate = debug $ "Shouter on " ++ show h ++ " terminated."
                     continue  = shouter h inbox

--------------------------
-- Per Document Threads --
--------------------------
snapshotter inbox shouter' document snapshot = do
                        instruction <- readChan inbox
                        case instruction of
                             TerminateSnapshotter -> terminate
                             NewOp op -> do let op_ts = drop (v) (reverse document)
                                                op' = transform op op_ts
                                                v = undefined --TODO
                                            shouter' <~ (Just $ encode op')
                                            continue (op:document) $ op <+ snapshot
                             SendSnapshot h -> send snapshot h
                      where
                        transform op (s:ss) = transform (fst $ t (op, s)) ss
                        transform op []     = op 
                        send sn h = do sendFrame h $ encode sn; return ()
                        continue  = snapshotter inbox shouter'
                        terminate = debug $ "Snapshotter terminated" 


operator title inbox = do shouter'   <- newChan :: IO (Chan (Maybe String))
                          snapshotter' <- newChan :: IO (Chan SnapshotterMessage)                 
                          forkIO (snapshotter snapshotter' shouter' [] [])
                          continue snapshotter' shouter'
                    where 
                      continue snapshotter' shouter' = do 
                            msg <- readChan inbox
                            case msg of 
                               Entry h   -> do socketThread h snapshotter' =<< (dupChan shouter'); continue snapshotter' shouter'
                               Exit  h   -> continue snapshotter' shouter'
                               TerminateOperator -> terminate
                      socketThread h snapshotter' shouter' = do 
                            debug $ "Starting new socket set on " ++ show h
                            packetsmasher' <- newChan :: IO (Chan (Maybe String))
                            forkIO $ packetsmasher packetsmasher' snapshotter'
                            forkIO $ listener h packetsmasher' shouter'
                            sendFrame h "ALL CLEAR"
                            forkIO $ shouter h shouter'
                      terminate = debug $ "operator for " ++ title ++ " terminated." 
                            

-------------------------
-- Application Threads --
-------------------------

accepter location port = do socket <- listenOn (PortNumber port)
                            continue M.empty socket
                            sClose socket    
                     where continue m socket = do
                              h <- acceptWeb socket
                              doc' <- readFrame h
                              case doc' of
                                 Left _    -> sendFrame h "REJECTED" >> continue m socket
                                 Right doc -> case M.lookup doc m of
                                                 Just chan -> Entry h ~> chan >> continue m socket
                                                 Nothing   -> do chan <- newChan :: IO (Chan OperatorMessage)
                                                                 forkIO $ operator doc chan
                                                                 Entry h ~> chan
                                                                 continue (M.insert doc chan m) socket 

server location port = withSocketsDo $ do
         debug $ "Starting Haskellpad server on port " ++ show port ++ ".."
         accepter location port


