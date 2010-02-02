-- Haskellpad
-- A collaborative code editor for students and haskellers
-- Inspired by the design of Google Wave's operational transform semantics.
-- Written by Liam O'Connor-Davis, with help from the rest of the Google Wave Team.
-- Released under the BSD3 License.

module Haskellpad.Server where

import Haskellpad.OperationalTransforms
import Haskellpad.Websockets

import Network 
import System.IO
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Text.JSON
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M

data OperatorMessage = Entry Handle
                     | Exit Handle
                     | Terminate 
                     deriving (Show, Eq)

(~>) = flip writeChan

(<~) :: Chan a -> a -> IO ()
(<~) = writeChan

debug :: String -> IO ()
debug = putStrLn

------------------------
-- Per Socket Threads -- 
------------------------

broadcaster to ops = do msg <- readChan to 
                        case msg of
                          Just v -> do case (decode v) :: Result Packet of
                                          Error e   -> debug $ "malformed packet: " ++ e ++ " (message was " ++ v ++ ")"
                                          Ok packet -> Just packet ~> ops
                                       continue
                          Nothing -> debug "Broadcaster terminated."
                   where continue = broadcaster to ops

listener h to from = do v <- readFrame h "" 
                        case v of
                          Right str  -> handle str >> continue    
                          Left str   -> handle str >> terminate
                   where terminate  = do Nothing ~> to
                                         Nothing ~> from
                                         debug $ "Listener on " ++ show h ++ " terminated."
                         continue   = listener h to from
                         handle str = when (not $ boring str) $ Just str ~> to
                         boring v   = any ($v) [all (==' '), ((/='[') . head)]

shouter h from = do v <- readChan from 
                    case v of 
                      Nothing  -> terminate
                      Just msg -> do v <- sendFrame h msg
                                     if v then continue else terminate
               where terminate = debug $ "Shouter on " ++ show h ++ " terminated."
                     continue  = shouter h from

--------------------------
-- Per Document Threads --
--------------------------

applicant ops from document version = do op_s <- readChan ops
                                         case op_s of
                                            Nothing -> terminate
                                            Just (v, op) -> do
                                               let op_ts = drop (v) (reverse document)
                                                   op'   = transform op op_ts
                                               debug $ "Document(" ++ show version ++ "): " ++ show document
                                               debug $ "Document*: " ++ (show $ normalize $ collapse document)
                                               from <~ (Just $ encode (version, op'))
                                               applicant ops from (op':document) $ version + 1
                                   where 
                                     transform op (s:ss) = transform (fst $ t (op, s)) ss
                                     transform op []     = op 
                                     terminate = debug $ "Applicant terminated"
                                     collapse list = foldr (<+) [] list

operator title inbox = do from <- newChan :: IO (Chan (Maybe String))
                          ops  <- newChan :: IO (Chan (Maybe Packet))                 
                          forkIO (applicant ops from [] (0::Int))
                          continue ops from
                    where 
                      continue ops from = do 
                            msg <- readChan inbox
                            case msg of 
                               Entry h   -> do socketThread h ops =<< (dupChan from); continue ops from
                               Exit  h   -> continue ops from
                               Terminate -> terminate
                      socketThread h ops from = do 
                            debug $ "Starting new socket set on " ++ show h
                            to <- newChan :: IO (Chan (Maybe String))
                            forkIO $ broadcaster to ops
                            forkIO $ listener h to from
                            sendFrame h "ALL CLEAR"
                            forkIO $ shouter h from
                      terminate = debug $ "operator for " ++ title ++ " terminated." 
                            

-------------------------
-- Application Threads --
-------------------------

accepter location port = do socket <- listenOn (PortNumber port)
                            continue M.empty socket
                            sClose socket    
                     where continue m socket = do
                          h <- acceptWeb socket location port
                          doc' <- readFrame h ""
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


