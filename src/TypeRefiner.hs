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
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
   

module TypeRefiner where

import Utilities
import qualified AgaTypes  as AT
import AgaMonad2

-- NEW: Import your parser types and functions
import LLMParser
  ( parseLLMResponse
  , TrLLMResult(..)
  , ResponseError(..)
  )

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

--------------------------------------------------------------------------------
-- ENV and STATE for the Type‐Refiner task
--------------------------------------------------------------------------------

data TrEnv =
  TrEnv
  { _domainDef             :: AgdaCode
  , _domainPrompt          :: Text
  , _candidatePromptTemplate :: Text  -- template with placeholders
  }

data TrState =
  TrState
    { _nlTypeDescription :: Text
    -- ^ The user’s natural‐language type description
    , _nlClarifications  :: [(Text,Text)]
    -- ^ (Question, userAnswer) pairs for clarifications
    }

$(makeLenses ''TrEnv)
$(makeLenses ''TrState)

--------------------------------------------------------------------------------
-- 1. The ADT of messages for our refining logic
--------------------------------------------------------------------------------

data TrMsg
  = ClrReq Text           -- ^ LLM needs clarifications: store the question text
  | NlTypeReq             -- ^ LLM requests or we ask for an NL type definition
  | SubmitNlTypeReq       -- ^ Attempt to parse an LLM response as code or clarifications
  | TypeCheckCandidate AgdaCode
  | UserAcpetanceReq
  | Done

--------------------------------------------------------------------------------
-- Our TypeRefiner as an AgaTask
--------------------------------------------------------------------------------

typeRefiner :: AgaRuntime (m TrEnv Text TrState) =>
                  AgaTask m TrEnv Text TrState TrMsg
typeRefiner = AgaTask
  { _firstStep = fs
  , _taskStep  = undefined --ts
  , _initState = TrState "" []
  }

--------------------------------------------------------------------------------
-- 2. The first step: ask the user for a type description
--------------------------------------------------------------------------------

fs :: AgaRuntime (m TrEnv Text TrState) => m TrEnv Text TrState (TrMsg, AgaCmd)
fs = do
  -- We start by requesting a type from the user
  return
    ( NlTypeReq
    , NextStep (UserReq "Explain the type you want to define:")
    )

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Fill your candidate template with the user’s NL type and the domain prompt.
-- getCandidatePrompt :: Text -> AgaMonad TrEnv Text TrState Text

-- getCandidatePrompt :: AgaRuntime (m TrEnv Text TrState) => Text -> m TrEnv Text TrState Text
getCandidatePrompt nld = do
  tmpl <- view (taskEnv . candidatePromptTemplate)
  dp   <- view (taskEnv . domainPrompt)
  return $ fConvInput nld tmpl dp

-- | Convert an AgdaCode (file or inline snippet) to a short text summary.
agdaToNaturalLan :: AgdaCode -> Text
agdaToNaturalLan ac =
  case ac of
    AgdaFile f      -> "[Agda snippet from file: " <> T.pack f <> "]"
    AgdaSource code -> "[Agda snippet inline: " <> code <> "]"

-- | Check a user response to see if they accept (“ok”) or not
checkUserResponse :: Text -> Bool
checkUserResponse txt = T.toLower (T.strip txt) == "ok"

--------------------------------------------------------------------------------
-- 3. The main step function
--------------------------------------------------------------------------------

-- ts :: (TrMsg, AgaMsg) -> AgaMonad TrEnv Text TrState (TrMsg, AgaCmd)
ts = \case
  ------------------------------------------------------------------------------
  -- A) NlTypeReq => user must specify a type in NL
  ------------------------------------------------------------------------------
  (NlTypeReq, UserRes ur) -> do
    -- Store the user’s type description
    (taskState . nlTypeDescription) .= ur
    -- Build a prompt for the LLM
    cp <- getCandidatePrompt ur
    return (SubmitNlTypeReq, NextStep (LLMReq cp))

  ------------------------------------------------------------------------------
  -- B) We got an LLMRes while waiting for it -> parse the response
  ------------------------------------------------------------------------------
  (SubmitNlTypeReq, LLMRes rT) -> do
    case parseLLMResponse rT of
      ----------------------------------------------------------
      -- B1) Single code block => go type‐check
      ----------------------------------------------------------
      Right (TrLLMAgdaSnippet code) -> do
        let agdaCode = AgdaSource code
        return (TypeCheckCandidate agdaCode, NextStep (TCReq agdaCode))

      ----------------------------------------------------------
      -- B2) LLM wants clarifications => transition to ClrReq
      ----------------------------------------------------------
      Right (TrLLMClarifications clarTxt) -> do
        -- We move to ClrReq to let the user respond specifically
        return
          ( ClrReq clarTxt
          , NextStep (UserReq ("LLM is asking for clarifications:\n"
                     <> clarTxt
                     <> "\nPlease respond to the above question(s)."))
          )

      ----------------------------------------------------------
      -- B3) Parser errors
      ----------------------------------------------------------
      Left MultipleCodeBlocks -> rePrompt "Multiple code blocks, please provide exactly one."
      Left MalformedCodeBlock -> rePrompt "Malformed code block found."
      Left NoRecognizedContent-> rePrompt "No recognized content, please try again."

    where
      rePrompt :: Text -> AgaMonad TrEnv Text TrState (TrMsg, AgaCmd)
      rePrompt msg = do
        -- Possibly show the user or LLM that we want a single code block
        ur <- use (taskState . nlTypeDescription)
        newPrompt <- getCandidatePrompt ur
        let hint = "\n[DEBUG: " <> msg <> "]"
        return (SubmitNlTypeReq, NextStep (LLMReq (newPrompt <> hint)))

  ------------------------------------------------------------------------------
  -- C) ClrReq => LLM asked user for clarifications; we store them and proceed
  ------------------------------------------------------------------------------
  (ClrReq questionTxt, UserRes userAnswer) -> do
    -- Store (question, userAnswer) in the clarifications
    modifying (taskState . nlClarifications) (\old -> old ++ [(questionTxt, userAnswer)])
    -- Now we can let the user refine or restate their type:
    return
      ( NlTypeReq
      , NextStep (UserReq "Thanks. Please restate or refine your type now.")
      )

  ------------------------------------------------------------------------------
  -- D) We tried to type‐check a candidate => see if it’s success or error
  ------------------------------------------------------------------------------
  (TypeCheckCandidate ac, TCRes tcr) ->
    case tcr of
      TCSucces -> do
        return
          ( UserAcpetanceReq
          , NextStep (UserReq
               ( "The code type‐checked successfully.\nDo you accept?\n"
              <> "Type “ok” to finish or anything else to refine.\n"
              <> agdaToNaturalLan ac))
          )
      TCErr txt -> do
        -- Type‐check failed: we might want the LLM to refine
        ur <- use (taskState . nlTypeDescription)
        newPrompt <- getCandidatePrompt ur
        return (SubmitNlTypeReq, NextStep (LLMReq newPrompt))

  ------------------------------------------------------------------------------
  -- E) UserAcpetanceReq => user chooses “ok” to finalize or else refine again
  ------------------------------------------------------------------------------
  (UserAcpetanceReq, UserRes userTxt) -> do
    if checkUserResponse userTxt
      then return (Done, AgaStop)
      else do
        -- They want to refine again
        ur <- use (taskState . nlTypeDescription)
        newPrompt <- getCandidatePrompt ur
        return (SubmitNlTypeReq, NextStep (LLMReq newPrompt))

  ------------------------------------------------------------------------------
  -- F) Fallback => no route
  ------------------------------------------------------------------------------
  _ -> error "Unexpected transition in typeRefiner."
