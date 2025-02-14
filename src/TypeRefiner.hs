{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE TemplateHaskell #-}


module TypeRefiner where

import Utilities
import qualified AgaTypes  as AT

import AgaMonad 


import Control.Lens hiding (element)
import Control.Lens.TH
import Data.Text as T
import Data.Text.Encoding as T
import Data.Text.IO as T
import Control.Monad.Trans.RWS
import Control.Monad.Reader
import Control.Concurrent
import System.Environment
import Data.Text
import System.IO

import qualified Data.Aeson as A
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL


import Network.HTTP.Client as NC
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status
import Network.HTTP.Client.MultipartFormData


data TrEnv =
  TrEnv
  { _domainDef :: AgdaCode,
    _domainPrompt :: Text,
    _candidatePromptTemplate :: Text -- templatka  z polem domain prompt i polem data
  }

data  TrState =
 TrState
    { _nlTypeDescription :: Text,
      _nlClarifications :: [(Text,Text)]
    }

$(makeLenses ''TrEnv)
$(makeLenses ''TrState)

data TrMsg = ClrReq | TypeCheckCandidate AgdaCode |  NlTypeReq | SubmitNlTypeReq | UserAcpetanceReq | Done


typeRefiner :: AgaTask TrEnv Text TrState TrMsg
typeRefiner = AgaTask { _firstStep = fs
                      , _taskStep = ts
                      , _initState = TrState "" []}

  where

    getCandidatePrompt :: Text -> AgaMonad TrEnv Text TrState Text
    getCandidatePrompt nld = do
      tmpl <- view (taskEnv . candidatePromptTemplate)
      dp <- view (taskEnv . domainPrompt)
      return $ fConvInput nld tmpl dp
    fs = return
           (NlTypeReq,
            NextStep ( UserReq ("explain the type you want to define") )
           )
    extractAgdaFromPrompt :: Text -> Maybe AgdaCode
    extractAgdaFromPrompt = fmap (AgdaSource .  T.pack) . plainCode . T.unpack 
    agdaToNaturalLan :: AgdaCode -> Text
    agdaToNaturalLan =
      \case
         AgdaFile f -> "[NN[" <> T.pack f <>  "]]"
         AgdaSource s ->  "[NN[" <> s <>  "]]"
 
    checkUserResponse :: Text -> Bool 
    checkUserResponse =
      \case
         "ok" -> True
         _ -> False
    ts = \case
      (NlTypeReq, UserRes ur) -> do
        (taskState . nlTypeDescription) .= ur
        cp <- getCandidatePrompt ur
        return (SubmitNlTypeReq, NextStep (LLMReq (cp )))
      (SubmitNlTypeReq, LLMRes rT) -> do
        let mac = (extractAgdaFromPrompt rT)
        case mac of
          Just ac -> return (TypeCheckCandidate ac, NextStep (TCReq ac))
          Nothing -> do
            ur <- use (taskState . nlTypeDescription)
            gcp <- getCandidatePrompt ur
            return  (SubmitNlTypeReq, NextStep (LLMReq gcp))
      (TypeCheckCandidate ac, TCRes tcr) ->
        case tcr  of
          TCSucces -> return (UserAcpetanceReq, NextStep (UserReq ("do you acept?:"<>(agdaToNaturalLan ac)) ))
          TCErr txt -> do
            ur <- use (taskState . nlTypeDescription)
            gcp <- getCandidatePrompt ur
            return (SubmitNlTypeReq, NextStep (LLMReq gcp))
      (UserAcpetanceReq, UserRes ur') -> do
         if checkUserResponse ur'
         then (return (Done, AgaStop))
         else do
          ur <- use (taskState . nlTypeDescription)
          gcp <- getCandidatePrompt ur
          (return  (SubmitNlTypeReq, NextStep (LLMReq gcp)))
      _  -> error "TODO"

