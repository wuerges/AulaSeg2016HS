{-# LANGUAGE Rank2Types #-}

module Main where

import Lib

import Control.Monad
import System.Environment

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
  [key, inputF, outputF] <- getArgs
  runResourceT $
    CB.sourceFile inputF $$ wrapConduit (ceasarConduit (read key)) =$ CB.sinkFile outputF


wrapConduit ::  Conduit Symbol (ResourceT IO) Symbol -> Conduit ByteString (ResourceT IO) ByteString
wrapConduit c = CL.concatMap unpack =$= c =$= CL.map singleton
