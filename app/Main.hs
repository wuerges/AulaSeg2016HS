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

import Data.ByteString ( ByteString, unpack, pack, empty )
import Data.Word ( Word8 )
import Data.Maybe

-- Functions related to Conduit
cipherToConduit :: a -> Cypher a -> Conduit ByteString (ResourceT IO) ByteString
cipherToConduit k c = do
  mbs <- CC.fold
  yield $ pack $ c k $ unpack mbs

main :: IO ()
main = do
  getArgs >>= parse

parse :: [String] -> IO ()

parse [mode, key, inputF, outputF] =
    case mode of
      "-v"        -> version
      "ceasar"    -> runConduit $ cipherToConduit key (ceasar . ceasarKey)
      "ceasardec" -> runConduit $ cipherToConduit key (ceasar . ceasarInverseKey . ceasarKey)
      _           -> error "Unknown mode"

  where
    runConduit :: Conduit ByteString (ResourceT IO) ByteString -> IO ()
    runConduit conduit = runResourceT $ CB.sourceFile inputF $$ conduit =$ CB.sinkFile outputF


parse _ = usage >> exit

usage   = putStrLn "Usage: cyphers ceasar <key> <inputFile> <outputFile>"
version = putStrLn "Haskell cyphers 0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
