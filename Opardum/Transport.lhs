% Opardum                                                  
% A Collaborative Code Editor 
% Written by Liam O'Connor-Davis with assistance from the
% rest of the Google Wave Team.
% Released under BSD3 License
%include lhs.include
\begin{document}
\ignore{

> {-# OPTIONS_GHC -fno-warn-orphans #-}

}
\title{Opardum: Transport}
\maketitle

\section {Description}

This module defines the format in which data is transferred between client and server.

Currently the marshalling technique used is simple JSON (JavaScript Object Notatation), because it
can be intepreted on the client-side just by calling the JavaScript function @eval()@. 

This is an inefficient transport mechanism, however, and future implementations may revise it, 
particularly if the client can be rewritten in Haskell or a Haskell-like language.

\section {Implementation}

> module Opardum.Transport where
> import Opardum.OperationalTransforms
> import Opardum.ConcurrencyControl
> import Text.JSON

Because the JSON module defines numerous serializations for common types, we simply need to define
serializations for data types we create, namely |OpComponent|.

> instance JSON OpComponent where

For the deserialization operation, |readJSON|, we make use of the data types provided in the 
@Text.JSON@ library. We also use the |Maybe| monad, so that any failure in the deserialization
will short circuit the whole action without lots of explicit boilerplate logic.

>    readJSON (JSObject o) = maybe (Error "Malformed operation component") Ok $
>       let assocs = fromJSObject o in do 
>          JSString strdata <- lookup "data" assocs
>          JSString v       <- lookup "type" assocs
>          case fromJSString v of
>             "Insert" -> Just $ Insert $ fromJSString strdata
>             "Delete" -> Just $ Delete $ fromJSString strdata
>             "Retain" -> case (reads $ fromJSString strdata) of
>                            [(x,"")] -> return $ Retain x
>                            _        -> Nothing
>             _        -> Nothing
>    readJSON _ = Error "Unexpected JSON value"

Finally, we have a serialization operation, which encodes an operation component in JSON text.

>    showJSON = JSObject . toJSObject . show'
>         where show' (Insert str) = [("type",jss "Insert"),("data", jss str)]
>               show' (Delete str) = [("type",jss "Delete"),("data", jss str)]
>               show' (Retain v)   = [("type",jss "Retain"),("data", jss $ show v)]
>               jss = JSString . toJSString

Then, we expose serialize and deserialize functions to abstract the JSON from the rest of 
Opardum, so that it can be easily changed.

> serialize :: Packet -> String
> serialize = encode

> deserialize :: String -> Maybe Packet
> deserialize v = case decode v of
>                   Ok v'   -> Just v'
>                   Error _ -> Nothing

\end{document}
