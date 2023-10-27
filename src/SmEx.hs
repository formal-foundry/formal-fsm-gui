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


createFSM :: ByteString -> FSM String String
createFSM json = case  decode  json :: Maybe Value of
  Nothing -> FSM M.empty
  Just val -> case val of
    Object o -> case AK.lookup (K.fromString "states") o of
          Nothing -> FSM M.empty
          Just s -> case s of
            Object x -> FSM $ M.map createState (toStringFromKM x)
            _ -> FSM M.empty
     
    _ -> FSM M.empty


toStringFromKM :: KeyMap Value -> Map String Value
toStringFromKM km = M.mapKeys (\x -> K.toString x) (AK.toMap km)
 

createState :: Value -> State String String
createState v = case v of
  Object o ->  case AK.lookup (K.fromString "on") o of
     Just on -> case on of
       Object s -> State $ M.mapWithKey createTransition(toStringFromKM s)
     Nothing  -> State M.empty
  _ -> State M.empty

createTransition ::  String -> Value -> Transition String
createTransition k v = case v of
  Object vv -> 
    case (AK.!?) vv (K.fromString "target") of
      Nothing -> Transition k
      Just x -> case x of
        String txt -> Transition $ T.unpack txt
        _ -> Transition k
  _ -> Transition "f"


----------TESTSc------------
test :: Int
test  = M.size $ states $ createFSM ex1

-- test2 :: int
-- test2 = case  decode  json :: maybe value of
--   nothing -> m.size fsm m.empty
--   just val -> case val of
--     object o ->
--       let 
--         states = case ak.lookup (k.fromstring "states") o of
--           nothing -> m.size fsm m.empty
--           just s -> case s of
--             object x -> m.size fsm $ m.map createstate (tostringfromkm x)

--     _ -> 0


            
------------------------------------
-- createFSM :: ByteString -> FSM s t
-- createFSM json = case genValue json of
--     Object o ->
--       let 
--         states = case AK.lookup (K.fromString "states") o of
--           Nothing -> FSM M.empty
--           Just s -> case s of
--             Object x -> FSM $ M.map createState (toStringFromKM x)
--             _ -> FSM M.empty
--       in 
--        FSM M.empty
--     _ -> FSM M.empty


-- toStringFromKM :: KeyMap Value -> Map String Value
-- toStringFromKM km = M.mapKeys (\x -> K.toString x) (AK.toMap km)
 

-- createState :: Value -> State s t
-- createState v = case v of
--   Object o ->  case AK.lookup (K.fromString "on") o of
--      Just on -> case on of
--        Object s -> State M.empty
--      Nothing  -> State M.empty
--   _ -> State M.empty

-- createTransition :: forall s. Value -> Transition s
-- createTransition v = Transition {target = "State 3"}


-- genValue :: ByteString -> Value
-- genValue j =
--   case decode j :: Maybe Value of
--     Nothing -> Null
--     Just v -> v


