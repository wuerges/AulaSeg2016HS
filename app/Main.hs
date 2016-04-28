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
ceasarConduit :: Int -> Conduit Symbol (ResourceT IO) Symbol
ceasarConduit k = CL.map (ceasarSymbol k)

main :: IO ()
main = do
  getArgs >>= parse

wrapConduit ::  Conduit Symbol (ResourceT IO) Symbol -> Conduit ByteString (ResourceT IO) ByteString
wrapConduit c = CL.concatMap unpack =$= c =$= CL.map singleton

parse :: [String] -> IO ()

parse [mode, key, inputF, outputF] =
  runResourceT $ CB.sourceFile inputF $$ wrapConduit conduit =$ CB.sinkFile outputF
    where conduit = case mode of
            "-v"        -> version
            "ceasar"    -> ceasarConduit $ read key
            "ceasardec" -> ceasarConduit $ negate $ read key
            _           -> error "Unknown mode"


parse _ = usage >> exit

usage   = putStrLn "Usage: cyphers ceasar <key> <inputFile> <outputFile>"
version = putStrLn "Haskell cyphers 0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
