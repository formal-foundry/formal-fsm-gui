{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Utilities where

import AgaTypes
-- import AgaExtra
-- import AgdaApi

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
import Network.HTTP.Client.MultipartFormData

import Data.Text.Lazy             as TL
import Data.Text.Lazy.Encoding    as TL
import Data.Text.Lazy.IO          as TL

import Data.Text as T
import Data.Text.Encoding    as T
import Data.Text.IO          as T


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

fConvInput :: T.Text -> T.Text -> T.Text -> T.Text
fConvInput nld tmpl dp =
    let x1 = replaceText (T.unpack tmpl) "{nld}" (T.unpack nld) 
        x2 = replaceText  x1 "{dp}" (T.unpack dp)
    in  T.pack x2


replaceText :: String -> String -> String -> String
replaceText [] _ _ = []
replaceText str search replace
  | L.take (L.length search) str == search = replace ++ replaceText (L.drop (L.length search) str) search replace
  | otherwise = L.head str : replaceText (L.tail str) search replace


--------


gptConv ::  String -> [Message] -> String ->  IO  String
gptConv model prompt key = do
  let rprompt = L.reverse (trimPrompt prompt)
  manager <- newManager (tlsManagerSettings {managerResponseTimeout = responseTimeoutMicro 120000000 })
  let reqb = createGptRequest model rprompt key
  request <- return (createGptRequest model rprompt key)
  response <- httpLbs reqb manager
  let code = (statusCode $ responseStatus response)
  case code of
    200 -> do

      return $  ( decodeRes $ responseBody response)
    _ -> do
      die "Something went wrong, try one more time" 


trimPrompt :: [Message] -> [Message]
trimPrompt ml =
  let l = L.length ml in 
  if (L.length ml) <= 10 then ml
  else
    case ml of  
      (p1:p2:x) ->
            p1 : p2 : (L.drop (l - 6) ml)
      _ -> ml


createGptRequest :: String -> [Message] -> String -> Request
createGptRequest model prompt key = request
  where
    apiKey_ = B.pack key
    baseRequest = parseRequest_ "https://api.openai.com/v1/chat/completions"
    request = baseRequest
      { method = "POST"
      , requestHeaders = [ ("Content-Type", "application/json")
                         , ("Authorization", B.concat ["Bearer ", apiKey_])
                         ]
      , requestBody = RequestBodyLBS (genJsonReq model prompt)
      }


genJsonReq ::String -> [Message] -> BL.ByteString
genJsonReq model messages =
  encode $ A.object ["model" .=model, "messages" .= messages]


decodeRes :: BL.ByteString -> String
decodeRes r = case ((A.decode r ):: Maybe ChatCompletion ) of
                     Nothing -> "Somethig went wrong with GPT api request.."
                     Just x -> content $ message $ Prelude.head $ choices x


metaAgda :: B.ByteString
metaAgda = T.encodeUtf8 . T.pack $
  "{\"agdaVersion\": \"2.6.4\"dependencies\": []}"

-- {
--   "agdaVersion": "2.6.4",
--   "dependencies": []
-- }
