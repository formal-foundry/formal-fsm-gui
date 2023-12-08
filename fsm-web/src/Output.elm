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
  case m of
    Init _ _ -> divInit
    WaitingForAgdaFile i _ x z -> case  .empty x of
                                    True -> divInit
                                    False -> divF m 
    WaitingForAgdaCheck i _ x z -> case  .empty x of
                                    True -> divInit
                                    False -> divF m  
    DisplayResults i _ x z -> case  .empty x of
                                    True -> divInit
                                    False -> divF m 
  

divInit : Html Msg
divInit  = div [ style "width" "50%", style "border" "double"]
           [div [style "text-align" "center", style "border-bottom" "double", style "font-size" "large"]
              [text "Output Agda checker"],
           p [][text "Waiting for Agda Checker"]
           ]


divF : Model  -> Html Msg
divF m  =  div [ style "width" "50%", style "border" "double"]
              [divIMB m, divIVR m]


divIIVt : Model -> Html Msg
divIIVt m = div [][]

divIMB : Model ->  Html Msg
divIMB m =
  let
       g = case m of
           Init _ _ -> "bold"
           WaitingForAgdaFile _ _ _ RGeneral -> "bold"
           WaitingForAgdaFile _ _ _ _-> "normal"
           WaitingForAgdaCheck _ _ _ RGeneral -> "bold"
           WaitingForAgdaCheck _ _ _ _-> "normal"
           DisplayResults _ _ _ RGeneral -> "bold"
           DisplayResults _ _ _ _-> "normal"
       c =  case m of
           Init _ _ -> "normal"
           WaitingForAgdaFile _ _ _ RCode -> "bold"
           WaitingForAgdaFile _ _ _ _ -> "normal"
           WaitingForAgdaCheck _ _ _ RCode -> "bold"
           WaitingForAgdaCheck _ _ _ _-> "normal"
           DisplayResults _ _ _ RCode -> "bold"
           DisplayResults _ _  _ _-> "normal"
       a = case m of
           Init _ _ -> "normal"
           WaitingForAgdaFile _ _ _ Rall -> "bold"
           WaitingForAgdaFile _ _ _ _ -> "normal"
           WaitingForAgdaCheck _ _ _ Rall -> "bold"
           WaitingForAgdaCheck _ _ _ _-> "normal"
           DisplayResults _ _ _ Rall -> "bold"
           DisplayResults _ _ _ _-> "normal"
      
  in 
      div [ style "border-bottom" "double", style "padding-top" "4px"]
             [button [ style "padding" "3px", style "border-radius" "0% 20% 0% 0%", style "width" "100px", style "font-weight" g,
                       onClick (ChangeBookMarkO RGeneral)]
                      [ text "General"], 
               button [ style "padding" "3px",  style "border-radius" "0% 20% 0% 0%", style "width" "100px",  style "font-weight" c,
                       onClick (ChangeBookMarkO RCode)]
                      [ text "Only_code"],
               button [ style "padding" "3px",  style "border-radius" "0% 20% 0% 0%", style "width" "100px",  style "font-weight" a,
                       onClick (ChangeBookMarkO Rall)]
                      [ text "All_gpt"]
              ]


divIVR : Model -> Html Msg
divIVR m =
  case m of
  Init i b -> divInit
  WaitingForAgdaFile _ _ x y -> butChoiseR y x
  WaitingForAgdaCheck _ _ x y -> butChoiseR y x
  DisplayResults _ _ x y -> butChoiseR y x


butChoiseR : ResButton -> ResChecker -> Html Msg
butChoiseR b rc =
  let prefix = .path rc  in

  case b of
    RGeneral -> div [][text"f"]
    RCode -> div [][text "c"]
    Rall -> div [][text "a"]



