{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Main where

import AgaTypes
import Types
import CreateAgda
import AgaMain
import AgaExtra
import GHC.Generics
import Data.Aeson
import Data.Aeson.Text


import Data.List
import System.Console.CmdArgs
import System.Environment (getArgs)
import System.Process
import System.Console.ANSI
import System.Exit
import System.FilePath (splitFileName)
import System.Directory


import qualified NeatInterpolation as NI(text)

import Data.Text.Lazy
import Control.Monad.IO.Class (liftIO)

import Data.Text as T
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding    as TL

import Network.HTTP.Types.Status
import Network.Wai.Parse
import Web.Scotty
import Network.Wai.Middleware.Static

import Network.Wai
import Network.Wai.Middleware.Cors

import qualified Network.Wai.Parse as NWP
import System.Environment
import Data.Aeson
import Data.ByteString.Base64.Lazy as B64
import Control.Concurrent


addFrameHeader :: Middleware
addFrameHeader =
  modifyResponse (mapResponseHeaders (("Access-Control-Allow-Origin", "*") :))

addFrameHeaderA :: Middleware
addFrameHeaderA =
 (modifyResponse (mapResponseHeaders (("Access-Control-Allow-Methods", "*") :)))


addFrameHeaderC :: Middleware
addFrameHeaderC =
  modifyResponse (mapResponseHeaders (("Access-Control-Allow-Headers", "*") :))


mainAPI :: FSMEnv -> IO ()
mainAPI env =do 
  scotty (port env) $ do
    middleware addFrameHeader
    middleware addFrameHeaderA
    middleware addFrameHeaderC
    middleware $ staticPolicy (noDots >-> addBase (work_dir env))
    options (regex "/*")  $ text "Success"

    get "/help" $  text  $ TL.fromStrict infoWeb

    post "/checkAgda" $ do
      body <- jsonData :: ActionM ReqCheckAgda
      ts <- liftIO timestamp2
      liftIO $ preperDirStructure ts env 
      x <- liftIO $ forkIO $ loadAndR mainAG env (decodeB64 body) ts
      text $ TL.pack ts

    post "/getAgda" $ do
      body  <- jsonData :: ActionM ReqGetAgda
      let bs = case B64.decode  ( (TL.encodeUtf8 . TL.pack ) (schema body)) of
            Right x  -> x
            _ -> "empty"
          mo = case (mode body) of
               "pi" -> Pi
               _ -> Mb

      agda <-liftIO $ createAgdaFromBS bs mo
      text $ TL.pack agda


main :: IO ()
main = loadConfigAndRun mainAPI



loadConfigAndRun :: (FSMEnv  -> IO ()) -> IO ()
loadConfigAndRun mainAPI =
  do
    home <- getEnv "HOME"
    let path = home ++"/.fsm/config.json"
    mbCfg <- decodeFileStrict path :: IO (Maybe FSMEnv)
    case mbCfg of
      Nothing ->
                Prelude.putStrLn $ "Invalid JSON file format, check : " ++  path
      Just cfg ->  mainAPI cfg



preperDirStructure :: String -> FSMEnv  -> IO ()
preperDirStructure ts e = do
  setCurrentDirectory (work_dir e)
  createDirectory ts
  setCurrentDirectory $ (work_dir e)++"/"++ts
  appendFile "general.txt" "Waiting for update..."
  appendFile "code.txt" "Waiting for update..."
  appendFile "all.txt" "Waiting for update..."

decodeB64 :: ReqCheckAgda -> ReqCheckAgda
decodeB64 r =
  ReqCheckAgda
  {agdaCode = eitherB (agdaCode r)
  , prompt1 =  eitherB (prompt1 r)
  , prompt2 =  eitherB (prompt2 r)
  , turns = turns r
  , modelR =  modelR r
  }


eitherB :: String -> String
eitherB s = case B64.decode  ( (TL.encodeUtf8 . TL.pack ) (s)) of
              Right x  ->( TL.unpack . TL.decodeUtf8 ) x
              _ -> "empty"
-- B64.decode  ( (TL.encodeUtf8 . TL.pack ) (schema body))

infoWeb :: T.Text
infoWeb =  [NI.text|
EXAMPLE OF USAGE
text 
or HTML |]
