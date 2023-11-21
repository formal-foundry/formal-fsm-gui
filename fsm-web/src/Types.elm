module Types exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Http
import Browser.Navigation exposing (reload)
import Base64

import Json.Encode  as JE  exposing (..)
import Json.Decode exposing (Decoder, Error(..), decodeString, list, string)


type Model = Init Input MenuButton
           | WaitingForAgdaFile Input MenuButton ResChecker ResButton
           | WaitingForAgdaCheck Input MenuButton ResChecker ResButton
           | DisplayResults Input MenuButton ResChecker ResButton

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
         | Checker Model (Result Http.Error ResChecker)

type MenuButton = BSchema | BAgda | BP1 | BP2 | BSet

type ResButton  =  RGeneral | RA1 | RA2

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

type alias ResChecker = { newCode : String,
                        other : String}


resPathsDec : Decoder ResChecker
resPathsDec =
  Json.Decode.map2 ResChecker
    (Json.Decode.field "newCode" Json.Decode.string)
    (Json.Decode.field "other" Json.Decode.string)


resex : ResChecker
resex = { newCode = "Waiting for AGA ...",
          other = "some.."}


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
