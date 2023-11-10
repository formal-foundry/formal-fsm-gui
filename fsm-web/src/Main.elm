module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Http
import Browser.Navigation exposing (reload)
import Base64

-- import Html.Styled exposing (..)
-- import Html.Styled.Attributes exposing (css, href, src)
-- import Html.Styled.Events exposing (onClick)

-- import Css exposing (..) 
import Json.Encode  as JE  exposing (..)
import Json.Decode exposing (Decoder, Error(..), decodeString, list, string)

type Model =
             Init
           | Schema String
           | Agda String

type Msg
    = Reload
    | Paste String
    | Check
    | AgdaRes (Result Http.Error String)

init : () -> ( Model, Cmd Msg )
init _ =
    ( Init , Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
      Reload -> (Init, reload)
      Paste x -> (Schema x, Cmd.none)
      Check -> case model of
                 Schema x -> (model, agdaPost x)
                 _ -> (Init, Cmd.none)
      AgdaRes z -> case z of
                    Err e -> (Agda "SomeThing Went Wrong", Cmd.none)
                    Ok s -> (Agda s, Cmd.none)



view : Model -> Html Msg
view model = div [style "margin" "10px"][h1D, divP model, divB]



h1D : Html Msg
h1D = div [ style "display" "flex", style "align-items" "center"]
          [img [ src "http://localhost:8000/src/ff.jpg", height 150,
                 style "display" "inline"] [],
           h1  [ style "text-align" "center" , style "display" "inline",
                   style "margin-inline-start" "220px"]
               [ text "Finite-State Machine - Agda Checker"]
          ]

divP : Model ->  Html Msg
divP m = div [style "display" "flex", style "width" "100%"] [divI m, divO m]

divI : Model ->  Html Msg
divI m =
  let a = case m of
           Init -> []
           Schema s -> []
           Agda s -> []
           

  in    div [ style "width" "50%", style "border" "double"]
           [p [style "text-align" "center", style "border-bottom" "double",
               style "font-size" "large"]
              [text "Paste Schema Below"],
            textarea [style "width" "95%", style "margin-left" "4px",
                      style "height" "300px", 
                      onInput (\x -> Paste x)] a ]


divO : Model -> Html Msg
divO m =
  let pP = case m of
             Init ->  p [][]
             Schema x  -> p [style "margin-left" "5px"] [text x]
             Agda s -> p [style "margin-left" "5px"][text s]
  in
       div [ style "width" "50%", style "border" "double"]
           [p [style "text-align" "center", style "border-bottom" "double",
               style "font-size" "large"]
              [text "Output Agda checker"], pP]

divB : Html Msg
divB = div [style "display" "flex", style "justify-content" "space-around",
            style "margin-top" "20px"]
           [button [style"font-size" "large", style "padding" "7px",
                    onClick Check]
                   [text "Check"],

            button [style"font-size" "large", style "padding" "7px",
                    onClick Reload ]
                   [text "Clear"]]


inputArea : Html Msg
inputArea = div [][Html.textarea [onInput (\x -> Paste x)][]]



agdaPost : String ->  Cmd Msg
agdaPost s =
       
        Http.post
           { url = "http://localhost:3456/update"
           , body = Http.jsonBody (JE.object [("schema", JE.string (Base64.encode s)),
                                              ("info", JE.string "info Val")])
           , expect = Http.expectString AgdaRes 
           }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

