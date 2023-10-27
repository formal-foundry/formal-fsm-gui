{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -w #-}

module Types where

import Data.Map
import Data.String

data Transition s  = Transition {target :: s  }

-- instance IsString Transition s where
--   fromString s = Transition s


data State s t =  State { transitions :: Map t (Transition s)
                        }


data FSM s t = FSM { states :: Map s (State s t)
                   }

