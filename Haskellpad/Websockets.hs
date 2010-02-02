module Haskellpad.Websockets where

import System.IO
import System.IO.Error
import Data.Char (ord, chr)
import Network (listenOn, PortID(..), accept, withSocketsDo, sClose, Socket)

serverHandshake location port = 
    "HTTP/1.1 101 Web Socket Protocol Handshake\r\n\
    \Upgrade: WebSocket\r\n\
    \Connection: Upgrade\r\n\
    \WebSocket-Origin: http://" ++ location ++ "\r\n\
    \WebSocket-Location: ws://" ++ location ++ ":" ++ show port ++ "/\r\n\
    \WebSocket-Protocol: sample\r\n\r\n\0"
sendFrame :: Handle -> String -> IO Bool
sendFrame h s = do
  v <- try $ do
    hPutChar h (chr 0)
    hPutStr h s
    hPutChar h (chr 255)
  case v of Left _  -> return False
            Right _ -> return True

acceptWeb socket location port = do
                      (h,_,_) <- accept socket
                      hPutStr h (serverHandshake location port)
                      hSetBuffering h NoBuffering
                      return  h

readFrame :: Handle -> String -> IO (Either String String)
readFrame h str  = do                                           
  new' <- try $ hGetChar h
  case new' of 
     Left _    -> return $ Left str
     Right new -> case ord new of 
                     0   -> readFrame h "" 
                     255 -> return $ Right str
                     _   -> readFrame h (str++[new]) 
