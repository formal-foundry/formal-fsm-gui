{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -w #-}

module SmEx where

import Data.String.Interpolate 
import Data.Aeson as A
import Data.Aeson.KeyMap as AK
import Data.Aeson.Key as K
import Data.Aeson.Types as T
import Data.ByteString.Lazy as BSL
import Data.Map as M
import Data.Text as T

import Types




ex1 :: ByteString
ex1  = [i| {
  "id": "test1",
  "initial": "State 1",
  "states": {

"State 1": {
      "on": {
        "1-3 action": {
        "target": "State 3"
        },
        "1-1 action": {
          "internal": true
        }
      }
    },
    "State 3": {
      "on":  {
        "3-2 action": {
          "target": "State 2"
        },
        "3-1 action": {
          "target": "State 1"
        }
      }
    },
    "State 2": {
      "on": {
        "2-4 action": {
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

x :: Value -> String
x val=  case val of
    Object z -> case AK.lookup (K.fromString "id") z of
      Just e -> case e of
        String s -> T.unpack s
        _ -> "empty_schema_name"
    _ ->  "empty_schema_name"

getStates :: ByteString -> Value
getStates j = undefined 

convertInternal :: Value -> Value
convertInternal old =
  case old of
    Object o ->
      let 
        states = case AK.lookup (K.fromString "states") o of
          Nothing -> String "Bad Val"
          Just stat -> case stat of
                         Object s -> let z = AK.map id s
                                     in Object z
                         _ -> String "Bad Val"
      in
       String "Bad Val"
    _ -> String "f"  


genFSM :: ByteString -> Maybe (FSM s t)
genFSM j =
  case decode j :: Maybe Value of
    Nothing -> Nothing
    Just v -> 
           let nv = convertInternal v
               name =  x v
               states = "f"
               newJ = convertInternal v
            in
             Just  FSM {states = M.empty
                 , name = name
                 , initial = State {transitions = M.empty}

                 }


-- data Transition s  = Transition {target :: s  }




-- d ata State s t =  State { transitions :: Map t (Transition s)
--                          }

-- data FSM s t = FSM { states :: Map s (State s t)
--                    , name :: String
--                    , initial :: State s t 
--                    }



