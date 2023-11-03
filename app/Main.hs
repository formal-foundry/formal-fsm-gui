{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Main where

import Types
import SmEx

import qualified NeatInterpolation as NI(text)

import Data.Text.Lazy
import Control.Monad.IO.Class (liftIO)


import Data.Text as T
import Data.Text.Lazy as TL

import Network.HTTP.Types.Status

import Web.Scotty 
import qualified Network.Wai.Parse as NWP
import System.Environment
import Data.Aeson

mainAPI :: FSMEnv -> IO ()
mainAPI env =do 
  scotty (port env) $ do

    get "/help" $  text  $ TL.fromStrict info


    post "/jsonSchema" $ do
      js  <- body
      agda <-liftIO $ createAgda js
      -- text  $ TL.pack $ show agda
      text $ TL.pack agda



main :: IO ()
main = loadConfigAndRun mainAPI


loadConfigAndRun :: (FSMEnv  -> IO ()) -> IO ()
loadConfigAndRun mainAPI =
  do
  args <- getArgs
  case args of
     [configFileName] -> do
       mbCfg <- decodeFileStrict configFileName :: IO (Maybe FSMEnv)
       case mbCfg of
         Nothing ->
                   Prelude.putStrLn $ "Invalid JSON file format, check : " ++ configFileName
         Just cfg ->  mainAPI cfg
     _ ->
       Prelude.putStrLn $ show args ++  "\n\nInvalid number of arguments, please run the program again with one argument: config.json"


info :: T.Text
info =  [NI.text|
EXAMPLE OF USAGE
text 
or HTML |]
