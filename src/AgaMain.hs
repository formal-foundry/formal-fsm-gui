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

-- main :: IO ()
-- main = do
--   loadAndR mainAG


loadAndR :: (AGEnv  -> String -> IO ()) -> FSMEnv -> ReqCheckAgda -> String -> IO ()
loadAndR  mainAG fsmE req tsDir= do
  pwd  <- getEnv "PWD"

  let dirN = (work_dir fsmE) ++ tsDir
  problem <- extractProblem (agdaCode req)
  setCurrentDirectory (work_dir fsmE)
  createDirectory tsDir  
  --     problemlist <- runReaderT buildProblemList (args, (problemsDir c))
  --     writeFile (pwd++"/aga-log.txt") ( "Aga had  " ++ (show (length problemlist))++ " problems to solved\n\n")
  --     -- let mainDir = pwd ++ "/aga-exec"
      --     w16 = intToWord16 (threadsNumbers c) 
      -- createDirectory $ mainDir
      -- traverseConcurrently_ (ParN w16) (cAGE mainDir) problemlist
      -- where
      --    cAGE dir problem = do
      --      setCurrentDirectory dir
      --      let name = case stripPrefix (problemsDir c ++ "Problems/") (nameP problem) of
      --            Nothing -> "unknow_dir"
      --            Just x -> x
      --          nameOfProblemDir = replace "/" "-" (take (length name - 5 )name)
      --          dirN = dir ++ "/"  ++ nameOfProblemDir
      --          newAF = "AGA-" ++ "Problem.agda"
      --      createDirectory dirN
      --      setCurrentDirectory dirN
      --      writeFile (dirN++"/Problem.agda") (agdaP problem)
      --      threadDelay 12453
      --      copyFile (metaP problem) (dirN++"/Problem.json")
      --      threadDelay 12453
      --      copyFile "Problem.agda" "Org-Problem.agda"


  let  env = AGEnv
        { apiKey = gpt_key fsmE
        , orgAgdaF = dirN ++ "/Org-Problem.agda"
        , dirName = dirN
        , agdaFile = dirN ++ "/Problem.agda"
        , taskDescription = taskP problem
        , fullTask = fulltP problem
        , operationMode = PrettyMode
        , maxTurns = turns req
        , fGptTemp = prompt1 req
        , rGptTemp = prompt2 req
        , gptModel = modelR req
        , tc_url = tChecker_url fsmE
        , tc_key = tChecker_key fsmE
        , meta_l = ((work_dir fsmE)++ "/Problem.json")
        }
  mainAG env pwd



mainAG :: AGEnv -> String -> IO ()
mainAG env pwd = do
  checkAgdaF <- tryToCompileAPI   (agdaFile env) (meta_l env) (tc_url env)
  case checkAgdaF of
    Just x -> do
       cPrint  ("Incorrect  agda File:  " ++ (orgAgdaF env) ++ "\n\n" ++ "COMPILER ERROR: " ++ x ) Red
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
        cPrint "Too many attempts, Agda-GPT-Assistan fail. Increase max turn or change agda task for GPT. \n Check logs files." Red
        appendFile (pwd++"/aga-log.txt") ("\n FAILED " ++ " " ++ (dirName env))
    Nothing ->do
      setSGR [(SetColor Foreground Dull Green)]
      clearScreen
      setCursorPosition 0 0
      putStrLn $ "Compilation succeeded in " ++ (show l) ++ " attempts."
      appendFile (pwd++"/aga-log.txt") ("\n OK " ++ (show l) ++ "    " ++ (dirName env))
      setSGR [Reset]
      putStrLn $ (gpt_res (head state))
      threadDelay 2000000
      setSGR [Reset]

initInfo :: AGEnv ->  IO ()
initInfo env = do
  clearScreen
  setCursorPosition 0 0
  setSGR [(SetColor Foreground Dull Blue)]
  putStrLn "\n\n\n###############################################"
  putStrLn "Agda-GPT-Assistant started with the following flags:\n\n"
  setSGR [Reset]
  putStrLn $ "TASK:  " ++ (taskDescription env) ++ "\n\n"
  putStrLn $ "MODE:  " ++ (show (operationMode env)) ++ "\n\n"
  putStrLn $ "MAX TURN :  " ++ (show (maxTurns env)) ++ "\n\n"
  putStrLn $ "MODEL:  " ++ (gptModel env) ++ "\n\n"


intToWord16 :: Int -> Word16
intToWord16 n = fromIntegral (n `mod` fromIntegral (maxBound :: Word16))
