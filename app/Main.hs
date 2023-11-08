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

import Types
import SmEx


import GHC.Generics
import Data.Aeson
import Data.Aeson.Text

import qualified NeatInterpolation as NI(text)

import Data.Text.Lazy
import Control.Monad.IO.Class (liftIO)


import Data.Text as T
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding    as TL

import Network.HTTP.Types.Status

import Web.Scotty 
import qualified Network.Wai.Parse as NWP
import System.Environment
import Data.Aeson
import Data.ByteString.Base64.Lazy as B64

mainAPI :: FSMEnv -> IO ()
mainAPI env =do 
  scotty (port env) $ do

    get "/help" $  text  $ TL.fromStrict infoWeb


    post "/update" $ do
      body  <- jsonData :: ActionM ReqJson
      let bs = case B64.decode  ( (TL.encodeUtf8 . TL.pack ) (schema body)) of
            Right x  -> x
            _ -> "empty"
            
               
      liftIO $ Prelude.putStrLn $ (TL.unpack . TL.decodeUtf8 ) bs
      agda <-liftIO $ createAndCompileAgda bs
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


infoWeb :: T.Text
infoWeb =  [NI.text|
EXAMPLE OF USAGE
text 
or HTML |]
