{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Text


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
