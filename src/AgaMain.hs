{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

{-# OPTIONS_GHC -fno-cse #-}

module AgaMain  where

import Types
import AgaTypes
import qualified AgaGpt as G
import AgaExtra
import AgdaApi

import Control.Scheduler
import Control.Concurrent

import Data.List.Utils
import Data.Word (Word16)
import Data.ByteString.Base64.Lazy as B64

import Data.List
import System.Console.CmdArgs
import System.Environment (getArgs)
import System.Process
import System.Console.ANSI
import System.Exit
import System.FilePath (splitFileName)
import System.Directory

import Data.Aeson as A
import Control.Monad.Trans.RWS
import Control.Monad.Reader

import Control.Concurrent
import System.Environment

import System.IO


loadAndR :: (AGEnv  -> String -> IO ()) -> FSMEnv -> ReqCheckAgda -> String -> IO ()
loadAndR  mainAG fsmE req tsDir= do
  let dirN = (work_dir fsmE) ++ tsDir
  writeFile (dirN++"/Problem.agda") (agdaCode req)
  putStrLn "fr"
  putStrLn dirN
  problem <- extractProblem (dirN++"/Problem.agda")
  setCurrentDirectory dirN
  writeFile (dirN++"/gpt_f.txt") (prompt1 req)
  writeFile (dirN++"/gpt_r.txt") (prompt2 req)
  threadDelay 12453
  copyFile "Problem.agda" "Org-Problem.agda"

  let  env = AGEnv
        { apiKey = gpt_key fsmE
        , orgAgdaF = dirN ++ "/Org-Problem.agda"
        , dirName = dirN
        , agdaFile = dirN ++ "/Problem.agda"
        , taskDescription = goalR req
        , fullTask = goalR req
        , operationMode = PrettyMode
        , maxTurns = turns req
        , fGptTemp = dirN ++ "/gpt_f.txt"
        , rGptTemp = dirN ++ "/gpt_r.txt"
        , gptModel = modelR req
        , tc_url = tChecker_url fsmE
        , tc_key = tChecker_key fsmE
        , meta_l = metaP problem
        }
  mainAG env dirN




mainAG :: AGEnv -> String -> IO ()
mainAG env pwd = do
  checkAgdaF <- tryToCompileAPI   (agdaFile env) (meta_l env) (tc_url env)
  case checkAgdaF of
    Just x -> do
       appendFile (pwd ++ "/general.txt")  ("\n\nIncorrect  agda File:  " ++ (orgAgdaF env) ++ "\n\n" ++ "COMPILER ERROR: " ++ x )
    Nothing -> do
               initInfo env
               conversation env [] pwd


conversation :: AGEnv -> [ConvPart] -> String -> IO ()
conversation env cP pwd = do
  (mValue, state, _ ) <- runRWST G.debugMode env cP
  let l = length state
  case mValue of
    Just x ->
      if l  < (maxTurns env)
      then
        do
          conversation env state pwd
        else do
        appendFile (pwd ++ "/general.txt") ("\n\nToo many attempts, Agda-GPT-Assistan fail. \nIncrease max turn or change agda task for GPT. \n Check logs files.")

    Nothing ->do
      setSGR [(SetColor Foreground Dull Green)]
      clearScreen
      setCursorPosition 0 0
      appendFile (pwd ++ "/general.txt") $ "\nCompilation succeeded in " ++ (show l) ++ " attempts."
      setSGR [Reset]
      threadDelay 2000000
      setSGR [Reset]

initInfo :: AGEnv ->  IO ()
initInfo env = do
  let a = appendFile (dirName env ++ "/general.txt")

  a "\n\n##############################"
  a "\n\nAgda-GPT-Assistant started:\n"
  a $ "TASK: \n " ++ (taskDescription env) ++ "\n"
  a $ "MAX TURN :  " ++ (show (maxTurns env)) ++ "\n"
  a $ "MODEL:  " ++ (gptModel env) ++ "\n"


intToWord16 :: Int -> Word16
intToWord16 n = fromIntegral (n `mod` fromIntegral (maxBound :: Word16))
