{-# LANGUAGE OverloadedStrings  #-}

module Main where
import Options.Generic
import Lib

main :: IO ()
main = do
  opts     <- unwrapRecord "fcall"
  someFunc opts
