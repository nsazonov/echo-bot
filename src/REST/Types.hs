{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module REST.Types where

import qualified Data.ByteString.Char8 as BC
import Network.HTTP.Simple (Request)

newtype Token = Token {unToken :: BC.ByteString} deriving (Show)

newtype Host = Host {unHost :: BC.ByteString}

newtype Timeout = Timeout {unTimeout :: Integer}

newtype Target = Target {unTarget :: Int} deriving (Show, Ord, Eq)

newtype Offset = Offset {unOffset :: Int} deriving (Enum)

instance Show Offset where
  show = show . unOffset

instance Show Timeout where
  show t = show $ unTimeout t

data RequestMethod = GET | POST deriving (Show) -- TODO: move this to Aeson.Extended

class EndpointBuilder a where
  request :: Token -> a -> Request
