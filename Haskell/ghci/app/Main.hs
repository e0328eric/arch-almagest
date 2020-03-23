module Main where

import           System.Process

main :: IO ()
main = callCommand "stack exec ghci"
