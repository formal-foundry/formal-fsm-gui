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




-- schemaDiv : Model -> Html Msg
-- schemaDiv m =
--   let s = case m of
--             Init i _ -> .jsonSchema i
--             WaitingForAgdaFile i _ _ _ -> .jsonSchema i
--             WaitingForAgdaCheck i _ _ _ -> .jsonSchema i
--             DisplayResults i _ _ _-> .jsonSchema i
--   in
--     div [style "height" "350px"]
--         [div [style "text-align" "center", style "border-bottom" "double",
--               style "font-size" "large"]
--              [text "Paste Schema Below"],
--          textarea [style "width" "95%", style "margin-left" "4px",
--                    style "height" "300px", onInput (updateJson m)]
--                   [text s] ]


-- updateJson : Model -> String -> Msg
-- updateJson m s = case m of
--   Init i _ -> UpdateSchema {i | jsonSchema = s}
--   WaitingForAgdaFile i _ _ _-> UpdateSchema {i | jsonSchema = s}
--   WaitingForAgdaCheck i _  _ _-> UpdateSchema {i | jsonSchema = s}
--   DisplayResults i _ _ _-> UpdateSchema {i | jsonSchema = s}


-- agdaDiv : Model -> Html Msg
-- agdaDiv m =
--   let s = case m of
--             Init i _ -> .agdaValue i
--             WaitingForAgdaFile i _ _ _-> .agdaValue i
--             WaitingForAgdaCheck i _ _ _-> .agdaValue i
--             DisplayResults i _ _ _-> .agdaValue i
--   in
--   case m of
--     Init _ _ -> div [style "height" "350px"] [text "Waiting For Agda Code ..."]
--     WaitingForAgdaFile _ _ _ _ -> div [style "height" "350px"] [text "Waiting For Agda Code ..."]
--     WaitingForAgdaCheck _ _ _ _ ->div [style "height" "350px"]
--         [div [style "text-align" "center", style "border-bottom" "double",
--               style "font-size" "large"]
--              [text "Edit Agda code below"],
--          div [][textarea [style "width" "95%", style "margin-left" "4px",
--                    style "height" "300px", onInput (updateAgda m)]
--                   [text s]] ]
--     DisplayResults _ _ _ _-> div [style "height" "350px"]
--                            [div [style "text-align" "center", style "border-bottom" "double",
--               style "font-size" "large"]
--              [text "Edit Agda code below"],
--         div [] [textarea [style "width" "95%", style "margin-left" "4px",
--                    style "height" "300px", onInput (updateAgda m)]
--                   [text s]] ]



-- updateAgda : Model -> String -> Msg
-- updateAgda m s = case m of
--   WaitingForAgdaCheck i _ _ _ -> UpdateAgda {i | agdaValue = s}
--   DisplayResults i _ _ _-> UpdateAgda {i | agdaValue = s}
--   _ -> Restart

-- p1Div : Model -> Html Msg
-- p1Div m =
--   let s = case m of
--             Init i _ -> "Waiting For Agda Code ..."
--             WaitingForAgdaFile i _ _ _-> "Waiting For Agda Code ..."
--             WaitingForAgdaCheck i _ _ _  -> .prompt1 i
--             DisplayResults i _ _ _-> .prompt1 i
--   in 
--   case m of
--   Init _ _ -> div [style "height" "350px"] [text "Waiting For Agda Code ..."]
--   WaitingForAgdaFile i _ _ _-> div [style "height" "350px"]
--                                 [text "Waiting For Agda Code ..."]
--   WaitingForAgdaCheck i _ _ _ -> div [style "height" "350px"]
--                                  [textarea [style "width" "95%",
--                                   style "margin-left" "4px", style "height" "300px",
--                                   onInput (updatep1 m)]
--                                            [text s]]
--   DisplayResults i _ _ _-> div [style "height" "350px"]
--                             [textarea [style "width" "95%",
--                                        style "margin-left" "4px", style "height" "300px",
--                                        onInput (updatep1 m)]
--                                            [text s]]

-- updatep1 : Model -> String -> Msg
-- updatep1 m s = case m of
--   WaitingForAgdaCheck i _ _ _  -> UpdateAgda {i | prompt1 = s}
--   DisplayResults i _ _ _-> UpdateAgda {i | prompt1 = s}
--   _ -> Restart

-- p2Div : Model -> Html Msg
-- p2Div m =
--   let s = case m of
--             Init i _ -> "Waiting For Agda Code ..."
--             WaitingForAgdaFile i _ _ _-> "Waiting For Agda Code ..."
--             WaitingForAgdaCheck i _ _ _ -> .prompt2 i
--             DisplayResults i _ _ _ -> .prompt2 i
--   in 
--   case m of
--   Init _ _ -> div [style "height" "350px"] [text "Waiting For Agda Code ..."]
--   WaitingForAgdaFile i _ _ _-> div [style "height" "350px"]
--                                 [text "Waiting For Agda Code ..."]
--   WaitingForAgdaCheck i _ _ _->div [] [div [style "height" "350px"]
--                                  [textarea [style "width" "95%",
--                                   style "margin-left" "4px", style "height" "300px",
--                                   onInput (updatep2 m)]
--                                            [text s]]]
--   DisplayResults i _ _ _-> div [] [div [style "height" "350px"]
--                             [textarea [style "width" "95%",
--                                        style "margin-left" "4px", style "height" "300px",
--                                        onInput (updatep2 m)]
--                                            [text s]]]



-- updatep2 : Model -> String -> Msg
-- updatep2 m s = case m of
--   WaitingForAgdaCheck i _ _ _  -> UpdateAgda {i | prompt2 = s}
--   DisplayResults i _ _ _-> UpdateAgda {i | prompt2 = s}
--   _ -> Restart

-- setDiv : Model -> Html Msg
-- setDiv m = div [style "height" "350px"][ agdaSetDiv m, checkerSetDiv m ]

-- agdaSetDiv : Model -> Html Msg
-- agdaSetDiv m =
--     let t = case m of
--             Init i _ -> .codeMode (.setting i)
--             WaitingForAgdaFile i _ _ _-> .codeMode (.setting i)
--             WaitingForAgdaCheck i _ _ _ -> .codeMode (.setting i)
--             DisplayResults i _ _ _-> .codeMode (.setting i)
--   in
--   div [style "text-align" "center",
--                     style "font-size" "large",
--                     style "border-bottom" "double"]
--                    [p [style "font-size" "large", style "font-weight" "bold"]
--                    [text "agda set"],
--                     p [style "text-align" "left", style "font-size" "large" ]
--                       [text "Chose Agda file output (type : pi , mb)",
--                        div [][Html.input [type_ "text", value t, onInput (updateAC m )][]]

--                     ]]

-- updateAC : Model -> String -> Msg
-- updateAC m s = case m of
--   Init i _ ->  UpdateAM ((setSet <| setVmode s) i )
--   WaitingForAgdaFile i _ _ _ ->  UpdateAM ((setSet <| setVmode s) i )
--   WaitingForAgdaCheck i _ _ _->  UpdateAM ((setSet <| setVmode s) i )
--   DisplayResults i _ _ _ -> UpdateAM ((setSet <| setVmode s) i )
--    -- Init i _ -> UpdateAM { i | setting = (setVmode s (.setting i)) }

-- setSet : (ValS -> ValS)-> Input -> Input
-- setSet fn i  = {i | setting = fn i.setting } 


-- setVmode : String -> ValS -> ValS
-- setVmode  s v = {v | codeMode = s}

-- setVturns : String -> ValS -> ValS
-- setVturns  i v = {v | turns = i}

-- setVgpt : String -> ValS -> ValS
-- setVgpt  s v = {v | gpt = s}

-- updateG : Model -> String -> Msg
-- updateG m s = case m of
--   Init i _ ->  UpdateAM ((setSet <| setVgpt s) i )
--   WaitingForAgdaFile i _ _ _->  UpdateAM ((setSet <| setVgpt s) i )
--   WaitingForAgdaCheck i _ _ _->  UpdateAM ((setSet <| setVgpt s) i )
--   DisplayResults i _ _ _-> UpdateAM ((setSet <| setVgpt s) i )


-- updateT : Model -> String -> Msg
-- updateT m s  =
--   case m of
--   Init i _ ->  UpdateAM ((setSet <| setVturns s) i )
--   WaitingForAgdaFile i _ _ _->  UpdateAM ((setSet <| setVturns s) i )
--   WaitingForAgdaCheck i _ _ _->  UpdateAM ((setSet <| setVturns s) i )
--   DisplayResults i _ _ _-> UpdateAM ((setSet <| setVturns s) i )

-- checkerSetDiv : Model -> Html Msg
-- checkerSetDiv m =
--     let inp = case m of
--             Init i _ -> i
--             WaitingForAgdaFile i _ _ _  -> i
--             WaitingForAgdaCheck i _ _ _ -> i
--             DisplayResults i _ _ _-> i
--         k = .gpt (.setting inp)
--         it = .turns (.setting inp)
--   in
--   div [style "text-align" "center",
--                     style "font-size" "large",
--                     style "border-bottom" "double"]
--                    [p [style "font-size" "large", style "font-weight" "bold"]
--                    [text "checker set"],
--                     p [style "text-align" "left", style "font-size" "large" ]
--                       [text "Chose gpt modl (type : gpt-3.5-turbo , gtp-4)",
--                        div [style "margin" "0px 0px 20px 0px"][Html.input [type_ "text", value k, onInput (updateG m )][]],
--                        text "Chose number of turns (type : turns number)",
--                        div [][Html.input [type_ "text", value it, onInput (updateT m )][]]
--                     ]]



-- butChoise : MenuButton -> Model -> Html Msg
-- butChoise b m = case b of
--   BSchema -> schemaDiv m
--   BAgda -> agdaDiv m
--   BP1 -> p1Div m
--   BP2 -> p2Div m
--   BSet -> setDiv m
