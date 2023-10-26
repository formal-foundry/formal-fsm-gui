{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -w #-}

module Types where

import Data.Map


data Transition s  = Transition {target :: s  }

data State s t =  State { transitions :: Map t (Transition s)
                         }

data FSM s t = FSM { states :: Map s (State s t)
                   }
