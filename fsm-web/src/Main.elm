module Main exposing (..)

import Types exposing (..)
import Input exposing (..)
import Output exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Http
import Browser.Navigation exposing (reload)
import Base64
import Time
import Task

import Json.Encode  as JE  exposing (..)
import Json.Decode exposing (Decoder, Error(..), decodeString, list, string)



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
      Restart -> (Init initInput BSchema, reload)
      UpdateSchema i -> (updateS i model, Cmd.none)
      UpdateAgda i -> (updateDef i model, Cmd.none)
      UpdateGoal i -> (updateDef i model, Cmd.none)
      UpdateP1 i -> (updateDef i model, Cmd.none)
      UpdateP2 i -> (updateDef i model, Cmd.none)
      UpdateAM i -> (updateS i model, Cmd.none)
      UpdateT i ->  (updateDefq i model, Cmd.none)
      UpdateG i ->(updateDefq i model, Cmd.none)
      ChangeBookMark b -> (buttonChanger b model, Cmd.none)
      ChangeBookMarkO b -> (buttonChangerO b model, Cmd.none)
      GenAgda  -> (model, getAgda model)
      CheckAgda -> (model, checkAgda model)
      Generator m res -> (afterGen m res, Cmd.none)
      Checker m res -> (afterChk m res, Cmd.none)
      UpdateTxt mw x -> (mw, (checkGeneral mw))
      UpdateGeneral mw r -> (mw, checkCode mw (buildErrorMessage r))
      UpdateCode mw  gen r  -> (mw, checkAll mw gen (buildErrorMessage r))
      UpdateAll mw gen code r-> (updateG mw gen code r, Cmd.none)


updateGeneral : Model -> (Result Http.Error String) -> Model
updateGeneral m r =
   let res = buildErrorMessage r in
   case m of
    DisplayResults ei b x y k-> DisplayResults ei b x y {k | generalS = res}
    _ -> Init initInput BSchema

updateG : Model -> String -> String -> (Result Http.Error String) -> Model
updateG m gen code r =
   let res = buildErrorMessage r in
   case m of
    DisplayResults ei b x y k-> DisplayResults ei b x y {k | generalS = gen, codeS = code, allS = res }
    _ -> Init initInput BSchema



updateCode : Model -> (Result Http.Error String) -> Model
updateCode m r =
   let res = buildErrorMessage r in
   case m of
    DisplayResults ei b x y k-> DisplayResults ei b x y {k | codeS = res}
    _ -> Init initInput BSchema



updateAll : Model -> (Result Http.Error String) -> Model
updateAll m r =
   let res = buildErrorMessage r in
   case m of
    DisplayResults ei b x y k-> DisplayResults ei b x y {k |allS = "rvrvrere"}
    _ -> Init initInput BSchema




afterChk : Model -> (Result Http.Error String) -> Model
afterChk m r =
  let nR = buildErrorMessageChk r
  in
  case m of
    Init ei b  -> Init ei b 
    WaitingForAgdaFile ei b x y-> WaitingForAgdaFile ei b (ru nR) y
    WaitingForAgdaCheck ei b x y-> DisplayResults ei b (ru nR) y exStaticF
    DisplayResults ei b x y k-> DisplayResults ei b (ru nR) y k


ru : (Bool, String) -> ResChecker
ru (e,p) = { empty  = e,
          path = p}

afterGen : Model -> (Result Http.Error String) -> Model
afterGen m r =
  let newA = buildErrorMessage r
  in
  case m of
    WaitingForAgdaFile ei b x y -> WaitingForAgdaCheck {ei| agdaValue = newA} b x y
    _ -> Init initInput BSchema


-- --
updateDef : Input -> Model -> Model
updateDef i m =
 case m of
     Init ei b  -> Init i b 
     WaitingForAgdaFile ei b x y-> WaitingForAgdaFile i b x y
     WaitingForAgdaCheck ei b x y-> WaitingForAgdaCheck i b x y
     DisplayResults ei b x y k-> DisplayResults i b x y k

updateDefq : Input -> Model -> Model
updateDefq i m =
 case m of
     Init ei b  -> Init i b 
     WaitingForAgdaFile ei b x y-> WaitingForAgdaFile i b x y
     WaitingForAgdaCheck ei b x y-> WaitingForAgdaCheck i b x y
     DisplayResults ei b x y k-> WaitingForAgdaCheck i b x y


--
updateA : Input -> Model -> Model
updateA i m =
  case m of
       Init ei b -> Init ei b
       WaitingForAgdaFile ei b x y -> WaitingForAgdaFile i b x y
       WaitingForAgdaCheck ei b x y -> WaitingForAgdaCheck i b x y
       DisplayResults ei b x y k -> DisplayResults i b x y k 



updateS : Input -> Model -> Model
updateS i m =
  case m of
     Init ei b -> WaitingForAgdaFile i b  resex RGeneral
     WaitingForAgdaFile ei b x y -> WaitingForAgdaFile i b x y
     WaitingForAgdaCheck ei b x y  -> WaitingForAgdaFile i b x y 
     DisplayResults ei b x y k-> WaitingForAgdaFile i b x y 


buttonChanger : MenuButton -> Model -> Model
buttonChanger b m =
  case m of
   Init i g -> Init i b
   WaitingForAgdaFile i e x y -> WaitingForAgdaFile i b x y 
   WaitingForAgdaCheck i e x y -> WaitingForAgdaCheck i b x y
   DisplayResults i e x y k-> DisplayResults i b x y k 

buttonChangerO : ResButton -> Model -> Model
buttonChangerO b m =
  case m of
   Init i x -> Init i x 
   WaitingForAgdaFile i e x y -> WaitingForAgdaFile i e x y 
   WaitingForAgdaCheck i e x y -> WaitingForAgdaCheck i e x b
   DisplayResults i e x y k-> DisplayResults i e x b k


 -- ____VIEW ------

view : Model -> Html Msg
view model = div [style "margin" "10px"]
                 [headerDiv, contentDiv model, buttonsDiv model]



contentDiv : Model ->  Html Msg
contentDiv m = div [style "display" "flex", style "width" "100%"] [divI m, divO m]


buttonsDiv :Model ->  Html Msg
buttonsDiv m =
  let 
      agda = case m of
            WaitingForAgdaFile _ _ _ _-> False
            _ -> True
      check = case m of
             Init _ _ -> True
             WaitingForAgdaFile _ _ _ _ -> True
             WaitingForAgdaCheck _ _ _ _-> False
             DisplayResults _ _ _ _ _-> False
      clear = case m of
             Init _ _ -> True
             _ -> False

      

  in
  
        div [style "display" "flex", style "justify-content" "space-around",
            style "margin-top" "20px"]
           [button [style"font-size" "large", style "padding" "7px", disabled agda, onClick GenAgda]
                   [text "Generate Agda"],

            button [style"font-size" "large", style "padding" "7px", disabled clear, onClick Restart ]
                   [text "Clear"],
            button [style"font-size" "large", style "padding" "7px", disabled check, onClick CheckAgda]
                   [text "Check Agda"]
           ]

headerDiv : Html Msg
headerDiv = div [ style "display" "flex", style "align-items" "center"]
          [img [ src ("http://localhost:8000/src/ff.jpg"), height 150,          
                 style "display" "inline"] [],
           h1  [ style "text-align" "center" , style "display" "inline",
                   style "margin-inline-start" "220px"]
               [ text "Finite-State Machine - Agda Checker"]
          ]


-- REQUESTS --________________


getAgda : Model ->  Cmd Msg
getAgda m =
  let s = case m of
            WaitingForAgdaFile i _ _ _-> .jsonSchema i
            _ -> "pi"
      mo = case m of
            WaitingForAgdaFile i _ _ _->  .codeMode (.setting i)
            _ -> "pi"
  in 
        Http.post
           { url = server ++ "/getAgda"
           , body = Http.jsonBody (JE.object [("schema", JE.string (Base64.encode s)),
                                              ("mode", JE.string mo)])
           , expect = Http.expectString  (Generator m)
           }

checkAgda : Model -> Cmd Msg
checkAgda m =
  let ni = case m of
            Init i _ -> i
            WaitingForAgdaFile i _ _ _ -> i
            WaitingForAgdaCheck i _ _ _ -> i
            DisplayResults i _ _ _ _-> i
      p1 = ni.prompt1
      goal = ni.goal
      p2 = ni.prompt2
      code = ni.agdaValue
      gpt = ni.setting.gpt
      turns = case String.toInt ni.setting.turns of
                Just int -> int
                Nothing -> 2

  in 
        Http.post
           { url = server ++ "/checkAgda"
           , body = Http.jsonBody (JE.object
           [("agdaCode", JE.string (Base64.encode  code)),
            ("prompt1", JE.string (Base64.encode p1)),
            ("prompt2", JE.string (Base64.encode p2)),
            ("turns", JE.int turns),
            ("modelR", JE.string gpt),
            ("goalR", JE.string (Base64.encode goal))])
           , expect = Http.expectString  (Checker m) 
           }



checkGeneral : Model -> Cmd Msg
checkGeneral m =
  let ni = case m of
            Init i _ -> "empty"
            WaitingForAgdaFile i _ x _ -> .path x
            WaitingForAgdaCheck i _ x _ -> .path x
            DisplayResults i _ x _ _-> .path x
  in 
        Http.get
           { url = server ++ "/"++ ni ++ "/general.txt"
           , expect = Http.expectString  (UpdateGeneral m) 
           }




checkCode : Model -> String ->  Cmd Msg
checkCode m gen=
  let ni = case m of
            Init i _ -> "empty"
            WaitingForAgdaFile i _ x _ -> .path x
            WaitingForAgdaCheck i _ x _ -> .path x
            DisplayResults i _ x _ _-> .path x
  in 
        Http.get
           { url = server ++ "/"++ ni ++ "/code.txt"
           , expect = Http.expectString  (UpdateCode m gen) 
           }



checkAll : Model -> String -> String -> Cmd Msg
checkAll m gen code=
  let ni = case m of
            Init i _ -> "empty"
            WaitingForAgdaFile i _ x _ -> .path x
            WaitingForAgdaCheck i _ x _ -> .path x
            DisplayResults i _ x _ _-> .path x
  in 
        Http.get
           { url = server ++ "/"++ ni ++ "/all.txt"
           , expect = Http.expectString  (UpdateAll m gen code) 
           }




buildErrorMessage : (Result Http.Error String) -> String
buildErrorMessage result =
  case result of
    Ok s -> s
    Err httpError -> 
             case httpError of
                 Http.BadUrl message ->
                     message

                 Http.Timeout ->
                     "Server is taking too long to respond. Please try again later."

                 Http.NetworkError ->
                     "Unable to reach server."

                 Http.BadStatus statusCode ->
                     "Request failed with status code: " ++ String.fromInt statusCode

                 Http.BadBody message ->
                     message



buildErrorMessageChk : (Result Http.Error String) -> (Bool ,String)
buildErrorMessageChk result =
  case result of
    Ok s -> (False, s)
    Err httpError -> 
             case httpError of
                 Http.BadUrl message ->
                     (True, message)

                 Http.Timeout ->
                     (False, "Server is taking too long to respond. Please try again later.")

                 Http.NetworkError ->
                     (False, "Unable to reach server.")

                 Http.BadStatus statusCode ->
                     (False,("Request failed with status code: " ++ String.fromInt statusCode))

                 Http.BadBody message ->
                     (False, message)





main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = sub
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Init initInput BSchema, Cmd.none )


sub : Model -> Sub Msg
sub m =
  case m of
    DisplayResults ei b x y k-> Time.every 7000  (UpdateTxt m)
    _ -> Sub.none
