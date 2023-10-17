{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module SmEx where

import Data.String.Interpolate 
import Data.Aeson as A
import Data.Aeson.KeyMap as AK
import Data.Aeson.Key as K
import Data.Aeson.Types as T
import Data.ByteString.Lazy as BSL
import Data.Map

-- data Person = Person {
--       name :: String
--     , age  :: Int
--     } deriving Show

-- instance FromJSON Person where
--     parseJSON = withObject "Person" $ \v -> Person
--         <$> v .: "name"
--         <*> v .: "age"

data Transition s  = Transition {target :: s  }

data State s t =  State { transitions :: Map t (Transition s)
                         }

data FSM s t = FSM { states :: Map s (State s t)
                    }

ex1 :: ByteString
ex1  = [i| {
  "id": "test1",
  "initial": "State 1",
  "states": {
    "State 1": {
      "on": {
        "1-2 action": {
          "target": "State 3"
        },
        "1 -1 action": {
          "internal": true
        }
      }
    },
    "State 3": {
      "on": {
        "2-3 action": {
          "target": "State 2"
        },
        "2-1 action": {
          "target": "State 1"
        }
      }
    },
    "State 2": {
      "on": {
        "3-4 action": {
          "target": "State 4"
        }
      }
    },
    "State 4": {
      "on": {
        "4-3 action": {
          "target": "State 3"
        },
        "4-4 action": {
          "internal": true
        }
      }
    }
  }
}
|]


