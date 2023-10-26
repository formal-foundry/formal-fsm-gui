{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
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

-- data Transition s  = Transition {target :: s  }

-- data State s t =  State { transitions :: Map t (Transition s)
--                          }
-- data FSM s t = FSM { states :: Map s (State s t)
--                    , name :: String
--                    , initial :: State s t 
--                    }


w :: forall s t. FSM Key t 
w = let v = gen ex1
        sList = case v of
                  Object states -> AK.toMap states in
      -- FSM M.empty
      FSM $ M.mapWithKey  mapF sList


mapF :: s ->  Value   -> State s t
mapF key val =  case val of
  Object q -> let x = AK.lookup (K.fromString "on") q
    in
    case x of
     Just r -> case r of
       Object rr -> State $  M.mapWithKey mapFT (AK.toMap rr)
     Nothing  -> State M.empty
  _ -> State M.empty



mapFT :: t -> Value -> Transition s 
mapFT t v = undefined

-- mapFT :: t -> Value -> Transition s 
-- mapFT k v = undefined

-- mapF ::  Key -> Value -> Value
-- mapF k v  = let nV = case of
--                   Object x -> AK.toMap states
--                   _ -> M.empty in
--   undefined





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
            Object x -> Object $ AK.mapWithKey (\k a -> a) x 
            _ -> String "Bad Val"
      in
       String "Bad Val"
    _ -> String "f"  


-- genFSM :: ByteString -> Maybe (FSM s t)
-- genFSM j =
--   case decode j :: Maybe Value of
--     Nothing -> Nothing
--     Just v -> 
--            let nv = convertInternal v
--                name =  x v
--                states = "f"
--                newJ = convertInternal v
--             in
--              Just  FSM {states = M.empty
--                  , name = name
--                  , initial = State {transitions = M.empty}

--                  }


gen :: ByteString -> Value
gen j =
  case decode j :: Maybe Value of
    Nothing -> Null
    Just v -> v



