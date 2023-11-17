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


type Model = Init Input MenuButton
           | WaitingForAgdaFile Input MenuButton
           | WaitingForAgdaCheck Input MenuButton
           | DisplayResults Input MenuButton

type MenuButton = BSchema | BAgda | BP1 | BP2 | BSet

type alias Input = { jsonSchema  : String,
                     agdaValue : String,
                     prompt1 :  String,
                     prompt2 :  String,
                     checkerRes : Maybe String,
                     setting : ValS
                   }

type alias ValS = {codeMode :  String,
                      turns :  String,
                      gpt : String
                  }


type Msg = Restart
         | UpdateSchema Input
         | UpdateAgda Input
         | UpdateP1 Input
         | UpdateP2 Input
         | UpdateAM Input
         | UpdateT Input
         | UpdateG Input
         | ChangeBookMark MenuButton
         | GenAgda
         | CheckAgda
         | Generator Model (Result Http.Error String)
         | Checker Model (Result Http.Error String)

initInput : Input
initInput = { jsonSchema = ex,
              agdaValue = "Waiting For Agda Code ...",
              prompt1 = exp1,
              prompt2 = exp2,
              checkerRes = Nothing,
              setting = initSet }



initSet : ValS
initSet = {codeMode = "pi",
           turns = "5",
           gpt =  "gpt-3.5-turbo" }

init : () -> ( Model, Cmd Msg )
init _ =
    ( Init initInput BSchema, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
      Restart -> (Init initInput BSchema, reload)
      UpdateSchema i -> (updateS i model, Cmd.none)
      UpdateAgda i -> (updateA i model, Cmd.none)
      UpdateP1 i -> (updateDef i model, Cmd.none)
      UpdateP2 i -> (updateDef i model, Cmd.none)
      UpdateAM i -> (updateDef i model, Cmd.none)
      UpdateT i ->  (updateDef i model, Cmd.none)
      UpdateG i ->(updateDef i model, Cmd.none)
      ChangeBookMark b -> (buttonChanger b model, Cmd.none)
      GenAgda  -> (model, getAgda model)
      CheckAgda -> (Init initInput BSchema, Cmd.none)
      Generator m res -> (afterGen m res, Cmd.none)
      Checker _ _ -> ( Init initInput BSchema, Cmd.none)



afterGen : Model -> (Result Http.Error String) -> Model
afterGen m r =
  let newA = buildErrorMessage r
  in
  case m of
    WaitingForAgdaFile ei b  -> WaitingForAgdaCheck {ei| agdaValue = newA} b 
    _ -> Init initInput BSchema

updateDef : Input -> Model -> Model
updateDef i m =
 case m of
     Init ei b -> Init i b
     WaitingForAgdaFile ei b -> WaitingForAgdaFile ei b
     WaitingForAgdaCheck ei b -> WaitingForAgdaCheck i b
     DisplayResults ei b -> DisplayResults i b



updateS : Input -> Model -> Model
updateS i m =
  case m of
     Init ei b -> WaitingForAgdaFile i b
     WaitingForAgdaFile ei b -> WaitingForAgdaFile i b
     WaitingForAgdaCheck ei b -> WaitingForAgdaFile i b
     DisplayResults ei b -> WaitingForAgdaFile i b

updateA : Input -> Model -> Model
updateA i m =
  case m of
       Init ei b -> Init ei b
       WaitingForAgdaFile ei b -> WaitingForAgdaFile ei b
       WaitingForAgdaCheck ei b -> WaitingForAgdaCheck i b
       DisplayResults ei b -> DisplayResults i b

buttonChanger : MenuButton -> Model -> Model
buttonChanger b m =
  case m of
   Init i _ -> Init i b
   WaitingForAgdaFile i e -> WaitingForAgdaFile i b
   WaitingForAgdaCheck i e -> WaitingForAgdaCheck i b
   DisplayResults i e -> DisplayResults i b


view : Model -> Html Msg
view model = div [style "margin" "10px"][h1D, divP model, divB model]



divP : Model ->  Html Msg
divP m = div [style "display" "flex", style "width" "100%"] [divI m, divO m]

divI : Model ->  Html Msg
divI m = div [ style "width" "50%", style "border" "double"]
             [divMB m, divIV m]


divMB :Model ->  Html Msg
divMB m =
  let
       s = case m of
           Init _ BSchema -> "bold"
           Init _ _ -> "normal"
           WaitingForAgdaFile _ BSchema -> "bold"
           WaitingForAgdaFile _ _ -> "normal"
           WaitingForAgdaCheck _ BSchema -> "bold"
           WaitingForAgdaCheck _ _ -> "normal"
           DisplayResults _ BSchema -> "bold"
           DisplayResults _ _ -> "normal"
       a =  case m of
           Init _ BAgda -> "bold"
           Init _ _ -> "normal"
           WaitingForAgdaFile _ BAgda -> "bold"
           WaitingForAgdaFile _ _ -> "normal"
           WaitingForAgdaCheck _ BAgda -> "bold"
           WaitingForAgdaCheck _ _ -> "normal"
           DisplayResults _ BAgda -> "bold"
           DisplayResults _ _ -> "normal"
       p1 = case m of
           Init _ BP1 -> "bold"
           Init _ _ -> "normal"
           WaitingForAgdaFile _ BP1 -> "bold"
           WaitingForAgdaFile _ _ -> "normal"
           WaitingForAgdaCheck _ BP1 -> "bold"
           WaitingForAgdaCheck _ _ -> "normal"
           DisplayResults _ BP1 -> "bold"
           DisplayResults _ _ -> "normal"
       p2 = case m of
           Init _ BP2 -> "bold"
           Init _ _ -> "normal"
           WaitingForAgdaFile _ BP2 -> "bold"
           WaitingForAgdaFile _ _ -> "normal"
           WaitingForAgdaCheck _ BP2 -> "bold"
           WaitingForAgdaCheck _ _ -> "normal"
           DisplayResults _ BP2 -> "bold"
           DisplayResults _ _ -> "normal"
       set = case m of
              Init _ BSet -> "bold"
              Init _ _ -> "normal"
              WaitingForAgdaFile _ BSet -> "bold"
              WaitingForAgdaFile _ _ -> "normal"
              WaitingForAgdaCheck _ BSet -> "bold"
              WaitingForAgdaCheck _ _ -> "normal"
              DisplayResults _ BSet -> "bold"
              DisplayResults _ _ -> "normal"
  in 
      div [ style "border-bottom" "double", style "padding-top" "4px"]
             [button [ style "padding" "3px", style "border-radius" "0% 20% 0% 0%", style "width" "100px", style "font-weight" s,
                       onClick (ChangeBookMark BSchema)]
                      [ text "Schema"], 
               button [ style "padding" "3px",  style "border-radius" "0% 20% 0% 0%", style "width" "100px",  style "font-weight" a,
                       onClick (ChangeBookMark BAgda)]
                      [ text "Agda_code"],
               button [ style "padding" "3px",  style "border-radius" "0% 20% 0% 0%", style "width" "100px",  style "font-weight" p1,
                       onClick (ChangeBookMark BP1)]
                      [ text "Prompt_1"],
               button [ style "padding" "3px",  style "border-radius" "0% 20% 0% 0%", style "width" "100px",  style "font-weight" p2,
                       onClick (ChangeBookMark BP2) ]
                      [ text "Prompt_2"],
              button [ style "padding" "3px",  style "border-radius" "0% 20% 0% 0%", style "width" "100px",  style "font-weight" set,
                       onClick (ChangeBookMark BSet) ]
                      [ text "Settings"]
              ]


divIV : Model -> Html Msg
divIV m =
  case m of
  Init i b -> butChoise b m
  WaitingForAgdaFile i b -> butChoise b m 
  WaitingForAgdaCheck i b -> butChoise b m 
  DisplayResults i b -> butChoise b m



schemaDiv : Model -> Html Msg
schemaDiv m =
  let s = case m of
            Init i _ -> .jsonSchema i
            WaitingForAgdaFile i _ -> .jsonSchema i
            WaitingForAgdaCheck i _  -> .jsonSchema i
            DisplayResults i _ -> .jsonSchema i
  in
    div [style "height" "350px"]
        [div [style "text-align" "center", style "border-bottom" "double",
              style "font-size" "large"]
             [text "Paste Schema Below"],
         textarea [style "width" "95%", style "margin-left" "4px",
                   style "height" "300px", onInput (updateJson m)]
                  [text s] ]


updateJson : Model -> String -> Msg
updateJson m s = case m of
  Init i _ -> UpdateSchema {i | jsonSchema = s}
  WaitingForAgdaFile i _ -> UpdateSchema {i | jsonSchema = s}
  WaitingForAgdaCheck i _  -> UpdateSchema {i | jsonSchema = s}
  DisplayResults i _ -> UpdateSchema {i | jsonSchema = s}


agdaDiv : Model -> Html Msg
agdaDiv m =
  let s = case m of
            Init i _ -> .agdaValue i
            WaitingForAgdaFile i _ -> .agdaValue i
            WaitingForAgdaCheck i _  -> .agdaValue i
            DisplayResults i _ -> .agdaValue i
  in
  case m of
    Init _ _ -> div [style "height" "350px"] [text "Waiting For Agda Code ..."]
    WaitingForAgdaFile _ _-> div [style "height" "350px"] [text "Waiting For Agda Code ..."]
    WaitingForAgdaCheck _ _  ->div [style "height" "350px"]
        [div [style "text-align" "center", style "border-bottom" "double",
              style "font-size" "large"]
             [text "Edit Agda code below"],
         div [][textarea [style "width" "95%", style "margin-left" "4px",
                   style "height" "300px", onInput (updateAgda m)]
                  [text s]] ]
    DisplayResults _ _ -> div [style "height" "350px"]
                           [div [style "text-align" "center", style "border-bottom" "double",
              style "font-size" "large"]
             [text "Edit Agda code below"],
        div [] [textarea [style "width" "95%", style "margin-left" "4px",
                   style "height" "300px", onInput (updateAgda m)]
                  [text s]] ]



updateAgda : Model -> String -> Msg
updateAgda m s = case m of
  WaitingForAgdaCheck i _  -> UpdateAgda {i | agdaValue = s}
  DisplayResults i _ -> UpdateAgda {i | agdaValue = s}
  _ -> Restart

p1Div : Model -> Html Msg
p1Div m =
  let s = case m of
            Init i _ -> "Waiting For Agda Code ..."
            WaitingForAgdaFile i _ -> "Waiting For Agda Code ..."
            WaitingForAgdaCheck i _  -> .prompt1 i
            DisplayResults i _ -> .prompt1 i
  in 
  case m of
  Init _ _ -> div [style "height" "350px"] [text "Waiting For Agda Code ..."]
  WaitingForAgdaFile i _ -> div [style "height" "350px"]
                                [text "Waiting For Agda Code ..."]
  WaitingForAgdaCheck i _ -> div [style "height" "350px"]
                                 [textarea [style "width" "95%",
                                  style "margin-left" "4px", style "height" "300px",
                                  onInput (updatep1 m)]
                                           [text s]]
  DisplayResults i _ -> div [style "height" "350px"]
                            [textarea [style "width" "95%",
                                       style "margin-left" "4px", style "height" "300px",
                                       onInput (updatep1 m)]
                                           [text s]]

updatep1 : Model -> String -> Msg
updatep1 m s = case m of
  WaitingForAgdaCheck i _  -> UpdateAgda {i | prompt1 = s}
  DisplayResults i _ -> UpdateAgda {i | prompt1 = s}
  _ -> Restart

p2Div : Model -> Html Msg
p2Div m =
  let s = case m of
            Init i _ -> "Waiting For Agda Code ..."
            WaitingForAgdaFile i _ -> "Waiting For Agda Code ..."
            WaitingForAgdaCheck i _  -> .prompt2 i
            DisplayResults i _ -> .prompt2 i
  in 
  case m of
  Init _ _ -> div [style "height" "350px"] [text "Waiting For Agda Code ..."]
  WaitingForAgdaFile i _ -> div [style "height" "350px"]
                                [text "Waiting For Agda Code ..."]
  WaitingForAgdaCheck i _ ->div [] [div [style "height" "350px"]
                                 [textarea [style "width" "95%",
                                  style "margin-left" "4px", style "height" "300px",
                                  onInput (updatep2 m)]
                                           [text s]]]
  DisplayResults i _ -> div [] [div [style "height" "350px"]
                            [textarea [style "width" "95%",
                                       style "margin-left" "4px", style "height" "300px",
                                       onInput (updatep2 m)]
                                           [text s]]]



updatep2 : Model -> String -> Msg
updatep2 m s = case m of
  WaitingForAgdaCheck i _  -> UpdateAgda {i | prompt2 = s}
  DisplayResults i _ -> UpdateAgda {i | prompt2 = s}
  _ -> Restart

setDiv : Model -> Html Msg
setDiv m = div [style "height" "350px"][ agdaSetDiv m, checkerSetDiv m ]

agdaSetDiv : Model -> Html Msg
agdaSetDiv m =
    let t = case m of
            Init i _ -> .codeMode (.setting i)
            WaitingForAgdaFile i _ -> .codeMode (.setting i)
            WaitingForAgdaCheck i _  -> .codeMode (.setting i)
            DisplayResults i _ -> .codeMode (.setting i)
  in
  div [style "text-align" "center",
                    style "font-size" "large",
                    style "border-bottom" "double"]
                   [p [style "font-size" "large", style "font-weight" "bold"]
                   [text "agda set"],
                    p [style "text-align" "left", style "font-size" "large" ]
                      [text "Chose Agda file output (type : pi , mb)",
                       div [][Html.input [type_ "text", value t, onInput (updateAC m )][]]

                    ]]

updateAC : Model -> String -> Msg
updateAC m s = case m of
  Init i _ ->  UpdateAM ((setSet <| setVmode s) i )
  WaitingForAgdaFile i _ ->  UpdateAM ((setSet <| setVmode s) i )
  WaitingForAgdaCheck i _ ->  UpdateAM ((setSet <| setVmode s) i )
  DisplayResults i _ -> UpdateAM ((setSet <| setVmode s) i )
   -- Init i _ -> UpdateAM { i | setting = (setVmode s (.setting i)) }

setSet : (ValS -> ValS)-> Input -> Input
setSet fn i  = {i | setting = fn i.setting } 


setVmode : String -> ValS -> ValS
setVmode  s v = {v | codeMode = s}

setVturns : String -> ValS -> ValS
setVturns  i v = {v | turns = i}

setVgpt : String -> ValS -> ValS
setVgpt  s v = {v | gpt = s}

updateG : Model -> String -> Msg
updateG m s = case m of
  Init i _ ->  UpdateAM ((setSet <| setVgpt s) i )
  WaitingForAgdaFile i _ ->  UpdateAM ((setSet <| setVgpt s) i )
  WaitingForAgdaCheck i _ ->  UpdateAM ((setSet <| setVgpt s) i )
  DisplayResults i _ -> UpdateAM ((setSet <| setVgpt s) i )


updateT : Model -> String -> Msg
updateT m s  =
  case m of
  Init i _ ->  UpdateAM ((setSet <| setVturns s) i )
  WaitingForAgdaFile i _ ->  UpdateAM ((setSet <| setVturns s) i )
  WaitingForAgdaCheck i _ ->  UpdateAM ((setSet <| setVturns s) i )
  DisplayResults i _ -> UpdateAM ((setSet <| setVturns s) i )

checkerSetDiv : Model -> Html Msg
checkerSetDiv m =
    let inp = case m of
            Init i _ -> i
            WaitingForAgdaFile i _ -> i
            WaitingForAgdaCheck i _  -> i
            DisplayResults i _ -> i
        k = .gpt (.setting inp)
        it = .turns (.setting inp)
  in
  div [style "text-align" "center",
                    style "font-size" "large",
                    style "border-bottom" "double"]
                   [p [style "font-size" "large", style "font-weight" "bold"]
                   [text "checker set"],
                    p [style "text-align" "left", style "font-size" "large" ]
                      [text "Chose gpt modl (type : gpt-3.5-turbo , gtp-4)",
                       div [style "margin" "0px 0px 20px 0px"][Html.input [type_ "text", value k, onInput (updateG m )][]],
                       text "Chose number of turns (type : turns number)",
                       div [][Html.input [type_ "text", value it, onInput (updateT m )][]]
                    ]]



butChoise : MenuButton -> Model -> Html Msg
butChoise b m = case b of
  BSchema -> schemaDiv m
  BAgda -> agdaDiv m
  BP1 -> p1Div m
  BP2 -> p2Div m
  BSet -> setDiv m

divO : Model -> Html Msg
divO m =
  let res = case m of
               Init _ _ -> "Waiting for Agda Checker"
               WaitingForAgdaFile i _ -> .agdaValue i
               WaitingForAgdaCheck i _ -> (.agdaValue i) 
               DisplayResults i _ -> getRes i
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


divB :Model ->  Html Msg
divB m =
  let 
      agda = case m of
            WaitingForAgdaFile _ _ -> False
            _ -> True
      check = case m of
             Init _ _ -> True
             WaitingForAgdaFile _ _ -> True
             WaitingForAgdaCheck _ _ -> False
             DisplayResults _ _ -> False
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
            button [style"font-size" "large", style "padding" "7px", disabled check, onClick Restart]
                   [text "Check Agda"]
           ]

h1D : Html Msg
h1D = div [ style "display" "flex", style "align-items" "center"]
          [img [ src "http://localhost:8000/src/ff.jpg", height 150,
                 style "display" "inline"] [],
           h1  [ style "text-align" "center" , style "display" "inline",
                   style "margin-inline-start" "220px"]
               [ text "Finite-State Machine - Agda Checker"]
          ] 


getAgda : Model ->  Cmd Msg
getAgda m =
  let s = case m of
            WaitingForAgdaFile i _ -> .jsonSchema i
            _ -> "pi"
      mo = case m of
            WaitingForAgdaFile i _ ->  .codeMode (.setting i)
            _ -> "pi"
  in 
        Http.post
           { url = "http://localhost:3456/getAgda"
           , body = Http.jsonBody (JE.object [("schema", JE.string (Base64.encode s)),
                                              ("mode", JE.string "pi")])
           , expect = Http.expectString  (Generator m)
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



main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



ex : String
ex ="""
{
  "id": "New Machine",
  "initial": "idle",
  "states": {
    "idle": {
      "on": {
        "insertCoin": {
          "target": "selecting"
        }
      }
    },
    "selecting": {
      "on": {
        "selectProduct": {
          "target": "paid"
        },
        "cancelSelecting": {
          "target": "idle"
        }
      }
    },
    "paid": {
      "on": {
        "dispensing": {
          "target": "dispensing"
        },
        "cancelPurchase": {
          "target": "selecting"
        }
      }
    },
    "dispensing": {
      "on": {
        "collectProduct": {
          "target": "idle"
        }
      }
    }
  }
}"""



exp1 : String
exp1 =
   """
Task: Implement a function with the following type signature in Agda:

{function_type}

Current Agda code:

{agda_code}

Please provide the exact Agda code that I need to append to the existing code to implement the function according to the given type >

from this momemn, I will always append your code to my code, please reemeber about this, this is important.

Do not duplicate my code, always print only the code that I need to add at the end of already existing code.

   """



exp2 : String
exp2 =
  """
Hi GPT, I tried implementing the function using the code you provided, but I encountered some errors during compilation. Here's the >

{agda_code_with_changes}

The compiler reported the following errors:

{compiler_errors}

Could you please help me fix these errors? Provide any necessary modifications to the existing code or additional Agda code to resol>
  """

