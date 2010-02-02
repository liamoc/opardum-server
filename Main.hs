module Main(main) where

import Haskellpad.Server

location = "localhost"
--location = "123.243.79.173"
port = 9876

main = server location port
