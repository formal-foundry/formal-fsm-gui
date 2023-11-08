{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -w #-}

-- https://stately.ai/registry/new

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

import Data.Text.Lazy             as TL
import Data.Text.Lazy.Encoding    as TL
import Data.Text.Lazy.IO          as TL

import System.Directory

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


exvm :: ByteString
exvm  = [i|{
  "id": "New Machine",
  "initial": "idle",
  "states": {
    "idle": {
      "on": {
        "insertCoin": {
          "target": "selecting"
        }
      }
    },
    "selecting": {
      "on": {
        "selectProduct": {
          "target": "paid"
        },
        "cancelSelecting": {
          "target": "idle"
        }
      }
    },
    "paid": {
      "on": {
        "dispensing": {
          "target": "dispensing"
        },
        "cancelPurchase": {
          "target": "selecting"
        }
      }
    },
    "dispensing": {
      "on": {
        "collectProduct": {
          "target": "idle"
        }
      }
    }
  }
}
|] 


createFSM :: ByteString -> IO (FSM String String)
createFSM json = do
  case  decode  json :: Maybe Value of
    Nothing -> return $ FSM M.empty
    Just val -> do
      -- Prelude.putStrLn $ show val -- test kolejności
      case val of
        Object o -> case AK.lookup (K.fromString "states") o of
          Nothing -> return $ FSM M.empty
          Just s -> case s of
            Object x -> return $ FSM $ M.mapWithKey createStateFSM (toStringFromKM x)
            _ -> return $ FSM M.empty

    _ -> return $ FSM M.empty  

createStateFSM :: String ->  Value -> State String String
createStateFSM k v = case v of
  Object o ->  case AK.lookup (K.fromString "on") o of
     Just on -> case on of
       Object s -> State $ M.mapWithKey (createTransitionFSM k)(toStringFromKM s)
     Nothing  -> State M.empty
  _ -> State M.empty

createTransitionFSM ::  String -> String -> Value -> Transition String
createTransitionFSM sk k v = case v of
  Object vv -> 
    case (AK.!?) vv (K.fromString "target") of
      Nothing -> Transition sk
      Just x -> case x of
        String txt -> Transition $ T.unpack txt
        _ -> Transition k
  _ -> Transition "f"


toStringFromKM :: KeyMap Value -> Map String Value
toStringFromKM km = M.mapKeys (\x -> K.toString x) (AK.toMap km)


createAndCompileAgda :: ByteString ->  IO String
createAndCompileAgda bs = do
  fsm  <- createFSM $ bs
  agda <- piAgda fsm
  saveAgda $ agda
  return agda

c :: ByteString ->  IO ()
c bs = do
  fsm  <- createFSM $ bs
  agda <- piAgda fsm
  Prelude.putStrLn agda
  return ()


saveAgda :: String -> IO ()
saveAgda agda = do
  homeDir <- getHomeDirectory
  let file = homeDir ++ "/agdaCompilation/Problem.agda"
  -- putStrLn file
  Prelude.writeFile file agda 
  return ()


maybeAgda :: FSM String String -> IO String
maybeAgda fsm = do 
  let
    stateL = L.map cC $ M.keys $ states fsm
    inputL = foldInputMaybe (M.elems (states fsm))
    types = "open import Agda.Builtin.Maybe\n\n"
        ++ "data State : Set where\n"
        ++ "  " ++ L.foldl1 (\x a->  x ++ " " ++ a  ) stateL ++ " : State\n\n"
        ++"data Input : Set where\n"
        ++ "  " ++ L.foldl1 (\x a->  x ++ " " ++ a  ) inputL ++ " : Input\n\n"
  update <- createUpdateMaybe fsm
  return $ types ++ update

foldInputMaybe ::  [State String String] -> [String]
foldInputMaybe ls = L.map cC
  $ L.foldl (\x a -> x ++ (M.keys $ transitions a)) [] ls


createUpdateMaybe :: FSM String String -> IO String
createUpdateMaybe fsm = do
  let
    sig = "update : State → Input → Maybe State\n"
    maped =
       M.elems( M.mapWithKey (\sk sv ->
        (M.elems(M.mapWithKey ((\stateK tk tv ->
        "update " ++cC stateK ++ " " ++ cC tk ++ " = just " ++ cC (target tv) ++ "\n"
                     ) sk) (transitions sv)) ++
          [("update " ++ cC sk ++  " _ = nothing\n")])
                     ) (states fsm))

  return $ sig ++ ( L.concat ( L.concat  maped))


cC :: [Char] -> String
cC (s:xs) = case C.isDigit s of
  False -> case C.isLower s of
    True -> L.filter (not . isSpace) (s:xs)
    False -> L.filter (not . isSpace)((C.toLower s):xs)
  True -> L.filter (not . isSpace) (s:xs)



piAgda :: FSM String String -> IO String
piAgda fsm = do
  let
    stateL = L.map cC $ M.keys $ states fsm
    inputL = createInputPi fsm
    update = createUpdatePi fsm 
    types = "open import Agda.Builtin.Equality\n"
      ++ "open import Agda.Builtin.Bool\n"
      ++ "open import Agda.Builtin.Unit\n"
      ++ "open import Agda.Builtin.Sigma\n"
      ++ "open import Agda.Builtin.List\n"
      ++ "open import Agda.Builtin.Maybe\n\n"
      ++ "data State : Set where\n"
      ++ "  " ++ L.foldl1 (\x a->  x ++ " " ++ a  ) stateL ++ " : State\n\n"
      ++ "data Input : State → Set where\n"
      ++ inputL ++ "\n\n"
      ++ "update : ∀ (x : State) → Input x → State\n"
      ++ update
  return types

createInputPi :: FSM String String -> String
createInputPi fsm =
  let maped =
       M.elems( M.mapWithKey (\sk sv ->
        (M.elems(M.mapWithKey ((\stateK tk tv ->
        "  " ++ cC tk ++ " : Input " ++ cC stateK ++ "\n"
        ) sk) (transitions sv))) ) (states fsm))

  in L.concat . L.concat $ maped

createUpdatePi :: FSM String String -> String
createUpdatePi fsm =
  let maped =
       M.elems( M.mapWithKey (\sk sv ->
        (M.elems(M.mapWithKey ((\stateK tk tv ->
        "update " ++ cC stateK  ++" "++ cC tk ++ " = " ++ cC (target tv)++ "\n"
        ) sk) (transitions sv))) ) (states fsm))

  in L.concat . L.concat $ maped


----------TESTS------------
-- test :: Int
-- test  = M.size $ states $ createFSM ex1


