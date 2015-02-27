#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Filesystem.Path.CurrentOS(encodeString)
import System.Environment (getArgs)

bin = "../detectWhite"
tolerance = "10"
threshold = 28000

main = sh $ do
  [sourcefolder] <- liftIO getArgs
  f <- ls . fromString $ sourcefolder
  let file = case toText f of
              Right g -> g
  score <- inproc bin [file,tolerance] yes
  liftIO $ echo score
  {-if (read . encodeString . fromText $ score) > threshold
     then do 
       liftIO $ echo (fromString yesfolder)
       inproc "cp" [file,fromString yesfolder] yes
     else do 
       liftIO $ echo (fromString nofolder)
       inproc "cp" [file,fromString nofolder] yes-}
