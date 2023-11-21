{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Text
import Data.ByteString.Lazy as BSL

import Data.Text as T
import Data.Text.Lazy as TL

import Data.Map
import Data.String

data Transition s  = Transition {target :: s  }

data State s t =  State { transitions :: Map t (Transition s)
                        }

data FSM s t = FSM { states :: Map s (State s t)
                   } 

data FSMEnv = FSMEnv { port :: Int
                     } deriving (Show, Generic, ToJSON, FromJSON)


data ReqGetAgda = ReqGetAgda {schema :: String
                    , mode :: String
                    } deriving (Show, Generic, ToJSON, FromJSON)

data Mode = Mb | Pi deriving (Show, Generic, ToJSON, FromJSON)

data ResponseTC = ResponseTC{ output :: String
                              , status :: Int} deriving (Show,  Generic, ToJSON, FromJSON)


data ReqCheckAgda = ReqCheckAgda {agdaCode :: String
                    , prompt1 :: String
                    , prompt2 :: String
                    , turns :: Int
                    , model :: String
                    } deriving (Show, Generic, ToJSON, FromJSON)

