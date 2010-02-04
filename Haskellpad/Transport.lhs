% Haskellpad                                                  
% A Collaborative Code Editor 
% Written by Liam O'Connor-Davis with assistance from the
% rest of the Google Wave Team.
% Released under BSD3 License
%include lhs.include
\begin{document}

\title{Haskellpad: Transport}
\maketitle

\section {Description}

This module defines the format in which data is transferred between client and server.

Currently the marshalling technique used is simple JSON (Javascript Object Notatation), because it
can be intepreted on the client-side just by calling the JavaScript function @eval()@. 

This is an inefficient transport mechanism, however, and future implementations may revise it, 
particularly if the client can be rewritten in Haskell or a Haskell-like language.

\section {Implementation}

> module Haskellpad.Transport where
> import Haskellpad.OperationalTransforms
> import Text.JSON

Because the JSON module defines numerous serializations for common types, we simply need to define
serializations for data types we create, namely |OpComponent|.

> instance JSON OpComponent where

For the deserialization operation, |readJSON|, we make use of the data types provided in the 
@Text.JSON@ library. We also use the |Maybe| monad, so that any failure in the deserialization
will short circuit the whole action without lots of explicit boilerplate logic.

>    readJSON (JSObject v) = maybe (Error "Malformed operation component") Ok $
>       let assocs = fromJSObject v in do 
>          JSString strdata <- lookup "data" assocs
>          JSString v       <- lookup "type" assocs
>          case fromJSString v of
>             "Insert" -> return $ Insert $ fromJSString strdata
>             "Delete" -> return $ Delete $ fromJSString strdata
>             "Retain" -> case (reads $ fromJSString strdata) of
>                            [(v,"")] -> return $ Retain v

Finally, we have a serialization operation, which encodes an operation component in JSON text.

>    showJSON = JSObject . toJSObject . zip ["type","data"] . map JSString . 
>               map toJSString . words . show' 
>         where show' (Insert str) = "Insert " ++ str
>               show' (Delete str) = "Delete " ++ str
>               show' v = show v

\end{document}
