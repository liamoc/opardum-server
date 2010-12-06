% Opardum
% A Collaborative Code Editor
% Written by Liam O'Connor-Davis with assistance from the
% rest of the Google Wave Team.
% Released under BSD3 License
%include lhs.include
\begin{document}
\title{Opardum: MD5 }
\maketitle
\section{Introduction}
This is a simple wrapper around @nano-md5@ which produces the checksum as a raw
|ByteString|.

> module Opardum.MD5 where

> import Data.Digest.OpenSSL.MD5 
> import qualified Data.ByteString.Unsafe as B (unsafeUseAsCStringLen)
> import qualified Data.ByteString as B
> import qualified Data.ByteString.Char8 as B2
> import Foreign
> import Foreign.C.Types

> md5rawsum :: B.ByteString -> B.ByteString
> md5rawsum p = unsafePerformIO $ B.unsafeUseAsCStringLen p $ \(ptr,n) -> do
>    allocaBytes md5_digest_length $ \digest_ptr -> do
>        digest  <- c_md5 ptr (fromIntegral n) digest_ptr
>        go digest 0 []
>  where
>    go :: Ptr Word8 -> Int -> [Word8] -> IO B.ByteString
>    go q n acc
>        | n `seq` q `seq` False = undefined
>        | n >= 16   = return $ B.pack $ reverse acc
>        | otherwise = do w <- peekElemOff q n
>                         go q (n+1) (draw w : acc)
>    draw w = fromIntegral w

\end{document}                          

