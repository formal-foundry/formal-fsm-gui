module Output exposing (..)

import Types exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Http
import Browser.Navigation exposing (reload)
import Base64

import Json.Encode  as JE  exposing (..)
import Json.Decode exposing (Decoder, Error(..), decodeString, list, string)

divO : Model -> Html Msg
divO m =
  let res = case m of
               Init _ _ -> "Waiting for Agda Checker"
               WaitingForAgdaFile i _ _ _ -> .agdaValue i
               WaitingForAgdaCheck i _ _ _-> (.agdaValue i) 
               DisplayResults i _ _ _ -> getRes i
  in
       div [ style "width" "50%", style "border" "double"]
           [div [style "text-align" "center", style "border-bottom" "double", style "font-size" "large"]
              [text "Output Agda checker"],
           p [][text res]
           ]

getRes : Input  -> String
getRes i = case  .checkerRes i  of
             Nothing -> "Waiting for Agda Checker"
             Just z -> z 
