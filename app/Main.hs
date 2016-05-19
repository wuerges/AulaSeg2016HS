{-# LANGUAGE Rank2Types #-}

module Main where

import Lib

import Control.Monad
import System.Environment
import System.Exit

import Control.Monad.Trans.Resource
import Data.Conduit

import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Binary as CB

import Data.ByteString ( ByteString, unpack, pack, singleton )
import Data.Word ( Word8 )
import Data.Maybe

-- Functions related to Conduit
ceasarConduit :: Symbol a => Int -> Conduit a (ResourceT IO) a
ceasarConduit k = CL.map (ceasarSymbol k)

main :: IO ()
main = do
  getArgs >>= parse

wrapConduit :: Conduit Word8 (ResourceT IO) Word8 -> Conduit ByteString (ResourceT IO) ByteString
wrapConduit c = CL.concatMap unpack =$= c =$= CL.map singleton

parse :: [String] -> IO ()

parse [mode, key, inputF, outputF] =
    case mode of
      "-v"        -> version
      "ceasar"    -> runConduit $ ceasarConduit $ read key
      "ceasardec" -> runConduit $ ceasarConduit $ negate $ read key
      _           -> error "Unknown mode"

  where
    runConduit conduit = runResourceT $ CB.sourceFile inputF $$ wrapConduit conduit =$ CB.sinkFile outputF


parse _ = usage >> exit

usage   = putStrLn "Usage: cyphers ceasar <key> <inputFile> <outputFile>"
version = putStrLn "Haskell cyphers 0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
