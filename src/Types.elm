import Browser
import Html exposing (..)
import Dict exposing (Dict)

-- main

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- model

type alias Model =
  { documentation:
    { names: Dict DefinitionID TranslatableString
    , descriptions: Dict DefinitionID TranslatableString
    , nibs: Dict NibID TranslatableString
    }
  , implementation: Dict DefinitionID Implementation
  }

--- documentation

type alias TranslatableString = Dict Language String
type alias Language = String

currentLanguage = "en"
makeTranslatable : String -> TranslatableString
makeTranslatable string = Dict.fromList [(currentLanguage, string)]
getTranslatable dict = Dict.get currentLanguage

--- implementation

type alias DefinitionID = String 
type alias NibID = String
type alias NodeID = String

type Implementation
  = ConstantImplementation String
  | ExternalImplementation String
  | GraphImplementation
    { connections: Dict NibConnection NibConnection
    , nodes: Dict NodeID Node
    }

type NibConnection
  = NodeConnection {nibID: NibID, nodeID: NodeID}
  | GraphConnection {nibID: NibID}

type Node = CallNode DefinitionID | ValueNode DefinitionID

--- extra
type alias Connection =
  { source: NibConnection
  , sink: NibConnection
  }

--- init

exampleID = "example"

init : () -> (Model, Cmd Msg)
init _ = 
  (
    { documentation=
      { names= Dict.fromList [(exampleID, makeTranslatable "Example")]
      , descriptions= Dict.fromList [(exampleID, makeTranslatable "Description of example")]
      , nibs= Dict.fromList []
      }
    , implementation=Dict.fromList [(exampleID, ConstantImplementation "5")]
    }
  , Cmd.none)

-- update

type Msg = Nothing

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

--view

view : Model -> Html Msg
view model =
  div [] [text "hello"]

-- type alias 



-- type alias FunctionDocumentation = 

-- type Documentation =
-- 	FunctionDocumentation 



-- names[definition_id] = translatedString
-- nib_names[nib_id] = translatedString
-- descriptions[definition_id] = translatedString
-- implementations[definition_id] = 
-- displays[definition_id]


-- model

