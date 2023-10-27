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
import Data.String
import Data.List as L
import Data.Char as C
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


createA :: IO ()
createA = do
  let fsm  = createFSM ex1
      stateL = L.reverse $ L.map sc $ M.keys $ states fsm
  putStrLn $ "module Problem where\n\n"
    ++ "data State : Set where\n"
    ++ "  " ++ L.foldl1 (\x a->  a ++ " " ++ x  ) stateL ++ " : State\n\n"
    ++"data Input : Set where\n" 

sc :: [Char] -> String
sc (s:xs) = case C.isDigit s of
  False -> case C.isLower s of
    True -> L.filter (not . isSpace) (s:xs)
    False -> L.filter (not . isSpace)((C.toLower s):xs)
  True -> L.filter (not . isSpace) (s:xs)


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


