import Browser
import Browser.Navigation as Nav
import Url exposing (Url)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, on, targetValue)
import Dict exposing (Dict)
import Random
import UUID exposing (UUID)
import Debug exposing (log)
import Json.Decode as Json

-- main

main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = ClickedLink
    , onUrlChange = ChangedUrl
    }

-- model

--- documentation

type alias TranslatableString = Dict Language String
type alias Language = String

currentLanguage = "en"

makeTranslatable : String -> TranslatableString
makeTranslatable string =
  Dict.fromList [(currentLanguage, string)]

getTranslatable : TranslatableString -> String
getTranslatable dict =
  Maybe.withDefault "" (Dict.get currentLanguage dict)

updateTranslatable : TranslatableString -> String -> TranslatableString
updateTranslatable dict string =
  Dict.insert currentLanguage string dict 


--- implementation

type alias DefinitionID = String 
type alias NibID = String
type alias NodeID = String

type Value
  = Integer Int
  | Number Float
  | Text String

valueToTypeString value =
  case value of
    Integer _ -> "integer"
    Number _ -> "number"
    Text _ -> "text"

type Implementation
  = ConstantImplementation Value
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

type alias Model =
  { names: Dict DefinitionID TranslatableString
  , descriptions: Dict DefinitionID TranslatableString
  , nibs: Dict NibID TranslatableString
  , implementations: Dict DefinitionID Implementation
  , navKey: Nav.Key
  , currentDefinitionID: Maybe DefinitionID
  }

exampleID = "695eec7b-3084-40e3-994b-59028c466d1d"
init : () -> Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key = 
  (
    { names= Dict.fromList [(exampleID, makeTranslatable "Example")]
    , descriptions= Dict.fromList [(exampleID, makeTranslatable "Description of example")]
    , nibs= Dict.fromList []
    , implementations=Dict.fromList
      [ (exampleID, ConstantImplementation (Number 9.8))
      ] 
    , navKey = key
    , currentDefinitionID = definitionIDFromUrl url
    }
  , Cmd.none)

-- update

type Msg
  = ChangedUrl Url
  | ClickedLink Browser.UrlRequest
  | NewConstant
  | ChangeName DefinitionID String
  | ChangeConstantValue DefinitionID Value
  | NoOp

initialSeed = Random.initialSeed 12345

isUuid string =
  case UUID.fromString string of
    Ok _ -> True
    Err _ -> False

definitionIDFromUrl : Url -> Maybe DefinitionID
definitionIDFromUrl url =
  case url.fragment of
    Nothing -> Nothing
    Just fragment ->
      if (isUuid fragment) then Just fragment else Nothing

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    NoOp ->
      ( model, Cmd.none )
    ChangedUrl url ->
      ( { model | currentDefinitionID = definitionIDFromUrl url }
      , Cmd.none
      )
    ClickedLink request ->
      (model, Cmd.none)
    NewConstant ->
      let (uuid, newSeed) = makeUuid initialSeed in
      ( { model
        | names =
          Dict.insert uuid (makeTranslatable "") model.names
        , implementations =
          Dict.insert uuid (ConstantImplementation (Text "")) model.implementations
        }
      , Nav.pushUrl model.navKey ("#" ++ uuid))
    ChangeName uuid newName ->
      ( { model
        | names =
          Dict.update uuid (\maybeName ->
            case maybeName of
              Nothing ->
                Just (makeTranslatable newName)
              Just name ->
                Just (updateTranslatable name newName)
          ) model.names
        }
      , Cmd.none
      )
    ChangeConstantValue uuid newValue ->
      ( { model | implementations = Dict.insert uuid (ConstantImplementation newValue) model.implementations }
      , Cmd.none
      )

makeUuid seed = 
  let (uuid, newSeed) = Random.step UUID.generator seed in (UUID.canonical uuid, newSeed)

-- editImplementations model edit =
--   { model | implementations = }

-- editNames model edit =
--   { model | names = edit model.name }

-- addName model uuid =
--   ( editNames model (\names ->
--       Dict.insert uuid (makeTranslatable "") names))

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

getName model definitionID =
  case Dict.get definitionID model.names of
    Nothing ->
      ""
    Just translatableString ->
      getTranslatable translatableString

--view

nameView model definitionID =
  input
    [ onInput (\text -> ChangeName definitionID text)
    , value (getName model definitionID)
    ] []

onChange : (String -> msg) -> Attribute msg
onChange tagger =
  on "input" (Json.map tagger targetValue)

view : Model -> Browser.Document Msg
view model =
  { title = "Nameless Programming Language"
  , body = 
    [ div [] 
      [ button [onClick NewConstant] [text "New Constant"]
      , case model.currentDefinitionID of
          Nothing ->
            div [] [text "Not editing a constant"]
          
          Just definitionID ->
            case Dict.get definitionID model.implementations of
              Nothing ->
                div [] [text "404"]
              Just implementation ->
                case implementation of
                  ConstantImplementation constantValue ->
                    let typeName = valueToTypeString constantValue in
                    div []
                    [ text ("Editing a constant: " ++ valueAsText constantValue)
                    , nameView model definitionID
                    , select [onChange (\type_ ->
                      ChangeConstantValue definitionID (
                          case type_ of
                            "text" ->
                              Text (valueAsText constantValue)

                            "integer" ->
                              case constantValue of
                                Number number ->
                                  Integer (round number)
                                _ ->
                                  case String.toInt (valueAsText constantValue) of
                                    Nothing ->
                                      Integer 0
                                    Just newNumber ->
                                      Integer newNumber
                            "number" ->
                              case String.toFloat (valueAsText constantValue) of
                                Nothing ->
                                  Number 0.0
                                Just newNumber ->
                                  Number newNumber
                            _ -> constantValue
                        ))] 
                      [ viewOption typeName "integer" "Integer"
                      , viewOption typeName "number" "Number"
                      , viewOption typeName "text" "Text"
                      ]
                    , case constantValue of
                        Text string ->
                          textarea
                            [ onChange (\value -> ChangeConstantValue definitionID (Text value))]
                            [text string]
                        Number number ->
                          input
                            [ type_ "number"
                            , step "any"
                            , value (String.fromFloat number)
                            , onChange (\value ->
                              case String.toFloat value of
                                Nothing ->
                                  NoOp
                                Just newNumber ->
                                  ChangeConstantValue definitionID (Number newNumber))
                            ] []
                        Integer integer ->
                          input
                            [ type_ "number"
                            , value (String.fromInt integer)
                            , onChange (\value ->
                              case String.toInt value of
                                Nothing ->
                                  NoOp
                                Just newInteger ->
                                  ChangeConstantValue definitionID (Integer newInteger))
                            ] []
                    ]
                  ExternalImplementation _ ->
                    div [] [text "External"]
                  GraphImplementation _ ->
                    div [] [text "Graph"]
      ]
    ]
  }

valueAsText : Value -> String
valueAsText value =
  case value of
    Text text ->
      text
    Number number ->
      String.fromFloat number
    Integer integer ->
      String.fromInt integer

viewOption selectedName name display =
  option [value name, selected (selectedName == name)] [text display]
