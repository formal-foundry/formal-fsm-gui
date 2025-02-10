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


module AgaMonad  where

import Utilities
import qualified AgaTypes  as AT

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


data StepCmd = LLMReq Text | TCReq AgdaCode | UserReq Text

data AgaCmd = NextStep StepCmd | AgaStop

data AgaMsg = LLMRes Text | TCRes TCMsg | UserRes Text

data TCMsg = TCSucces | TCErr Text

data AgaTask  eT oT sT mT =
  AgaTask
    { _firstStep ::  (AgaMonad eT oT sT (mT, AgaCmd)),
      _taskStep :: (mT,AgaMsg) -> (AgaMonad eT oT sT (mT, AgaCmd)),
      _initState :: sT
    }

-- aga state historia całej rozowy 

data AgaState sT = AgaState { _taskState :: sT
                            , _conversation :: [AT.Message]
                            }

data AgaENV eT = AgaENV { _taskEnv :: eT
                        , _apiGptKey :: Text
                        , _tcUrl :: Text
                        , _llmModel :: Text
                        }

data  AgaOutput oT = AgaOutput {_taskOutput :: oT}

type  AgaMonad eT oT sT  =
  RWST
   (AgaENV eT )
   ([AgaOutput oT])
   (AgaState sT)
   IO

data AgdaCode = AgdaSource Text | AgdaFile FilePath

$(makeLenses ''AgaTask)
$(makeLenses ''AgaState)
$(makeLenses ''AgaENV)
$(makeLenses ''AgaOutput)

runAgaCmd :: StepCmd -> AgaMonad eT oT sT AgaMsg
runAgaCmd  (LLMReq t) = do
  model <- view llmModel
  key <- view apiGptKey
  conv <- use conversation
  liftIO $ do
    llmRes <-  gptConv  (T.unpack model) conv (T.unpack key) 
    return (LLMRes (T.pack llmRes))

runAgaCmd  (TCReq ac) = do
  tcUrl <- view tcUrl
  liftIO $ do
    tcRes <- tryToCompileAPI ac metaAgda tcUrl
    return (TCRes tcRes)


runAgaCmd  (UserReq t) =  do
  let usRes = "user respons"
  return (UserRes usRes)


runAgaTask :: AgaTask eT oT sT mT -> AgaENV eT -> IO [oT]
runAgaTask at ae = fmap (\(_, ao ) -> fmap (\x -> x ^.taskOutput)  ao )
  (execRWST atm ae (AgaState (at ^. initState) []))
  where
    -- atLoop :: (mT,AgaCmd) -> (AgaMonad eT oT sT ())
    atLoop (_, AgaStop) = return ()
    atLoop (mt, NextStep ac) = do
      am <- runAgaCmd ac

      (at ^. taskStep) (mt, am) >>= atLoop

    -- atm :: AgaMonad eT oT sT ()
    atm =( at ^. firstStep) >>= atLoop





tryToCompileAPI :: AgdaCode -> B.ByteString -> Text -> IO TCMsg
tryToCompileAPI agda meta  url = do
  manager <- newManager defaultManagerSettings
  initialRequest <- parseRequest $ T.unpack url
  let
    request = initialRequest { method = "POST"}
    partList = case agda of
      AgdaSource tx -> [ partBS "Problem.agda" $  T.encodeUtf8 tx
                       , partBS "Problem.json"  meta
                       ]
      AgdaFile fp -> [ partFile "Problem.agda" fp
                     , partBS "Problem.json"  meta
                     ]

  req <- formDataBody partList request
  response <- httpLbs req manager
  res  <- dec $ responseBody response
  return $ res


dec :: BL.ByteString -> IO TCMsg
dec re = do
  let r = A.decode re :: Maybe AT.ResponseApi
  case r of
    Nothing -> return $ TCErr "No API respons"
    Just x -> do
      case AT.status x of
        0 -> return TCSucces
        _ -> return $ TCErr $ T.pack (AT.output x)


-- -- --------------------------------------

data TrEnv =
  TrEnv
  { _domainDef :: AgdaCode,
    _domainPrompt :: Text,
    _candidatePromptTemplate :: Text -- templatka  z polem domain prompt i polem nldesc
  }


data TrState =

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



-- ---- Utilities -- dependecies problem 

