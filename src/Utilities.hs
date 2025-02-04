{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Utilities where

import AgaTypes
-- import AgaExtra
import AgdaApi

import Control.Monad.Trans.RWS
import Control.Monad.IO.Class (liftIO)

import Data.Maybe
import Data.Aeson as A
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import System.Process as SP
import System.Console.ANSI
import System.Exit
import System.FilePath (splitFileName)
import System.Directory
import System.Environment (getEnv)

import Network.HTTP.Client as NC
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status
import Data.Text as T

import Control.Concurrent

plainCode :: String ->  Maybe String
plainCode res =
  let exR = extractCode  res
  in
  case exR of
    [] -> Nothing
    _ -> Just $ rmSubS "Agda" (rmSubS "agda" (L.concat $ exR))


extractCode :: String -> [String]
extractCode str = extractCodeBlocks' str []
  where extractCodeBlocks' [] acc = acc
        extractCodeBlocks' xs acc = let block = takeCodeBlock xs
                                    in case block of
                                      Nothing -> extractCodeBlocks' (L.drop 1 xs) acc
                                      Just (code, rest) -> extractCodeBlocks' rest (acc ++ [code])

        takeCodeBlock :: String -> Maybe (String, String)
        takeCodeBlock xs = if L.take 3 xs == "```"
                           then let (block, rest) = L.span (/= '`') (L.drop 3 xs)
                                in Just (block, L.drop 3 rest)
                           else Nothing

rmSubS :: String -> String -> String
rmSubS substr str = go str
  where go [] = []
        go s@(x:xs)
          | substr `L.isPrefixOf` s = L.drop (L.length substr) s
          | otherwise = x : go xs

fConvInput :: Text -> Text -> Text -> Text
fConvInput nld tmpl dp =
    let x1 = replaceText (T.unpack tmpl) "{nld}" (T.unpack nld) 
        x2 = replaceText  x1 "{dp}" (T.unpack dp)
    in  T.pack x2


replaceText :: String -> String -> String -> String
replaceText [] _ _ = []
replaceText str search replace
  | L.take (L.length search) str == search = replace ++ replaceText (L.drop (L.length search) str) search replace
  | otherwise = L.head str : replaceText (L.tail str) search replace


