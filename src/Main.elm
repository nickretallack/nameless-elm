port module Main exposing (..)
import Browser
import Browser.Navigation as Nav
import Url exposing (Url)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, on, targetValue)
import Dict exposing (Dict)
import Random exposing (Seed)
import UUID exposing (UUID)
import Debug exposing (log)
import Json.Decode
import Json.Encode
import Json.Decode.Pipeline exposing (required, optional, hardcoded)
import Time 

port saveState : Json.Encode.Value -> Cmd msg

-- main

main : Program String Model Msg
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

encodeTranslatable : TranslatableString -> Json.Encode.Value
encodeTranslatable dict =
  Json.Encode.dict identity Json.Encode.string dict

decodeTranslatable : Json.Decode.Decoder TranslatableString
decodeTranslatable =
  Json.Decode.dict Json.Decode.string

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
  | ExternalImplementation
  | GraphImplementation
    { connections: Dict NibConnection NibConnection
    , nodes: Dict NodeID Node
    }

encodeImplementation : Implementation -> Json.Encode.Value
encodeImplementation implementation =
  case implementation of
    ConstantImplementation value ->
      encodeConstant value
    GraphImplementation _ ->
      Json.Encode.string "TODO"
    ExternalImplementation ->
      Json.Encode.string "TODO"

decodeImplementation : Json.Decode.Decoder Implementation
decodeImplementation =
  Json.Decode.field "type" Json.Decode.string |> Json.Decode.andThen (\type_ ->
    case type_ of
      "constant" ->
        Json.Decode.map ConstantImplementation (Json.Decode.field "value" valueDecoder)
      _ -> Json.Decode.fail <| "Unknown implementation type: " ++ type_
  )

encodeConstant : Value -> Json.Encode.Value
encodeConstant value =
  Json.Encode.object
    [ ("type", Json.Encode.string "constant")
    , ("value",
        case value of
          Integer integer ->
            Json.Encode.object [ ("type", Json.Encode.string "integer"), ("value", Json.Encode.int integer) ]
          Number number ->
            Json.Encode.object [ ("type", Json.Encode.string "number"), ("value", Json.Encode.float number) ]
          Text text ->
            Json.Encode.object [ ("type", Json.Encode.string "text"), ("value", Json.Encode.string text) ]
      )
    ]

valueDecoder : Json.Decode.Decoder Value
valueDecoder =
  Json.Decode.field "type" Json.Decode.string |> Json.Decode.andThen (\type_ ->
    case type_ of
      "integer" ->
        Json.Decode.map Integer (Json.Decode.field "value" Json.Decode.int) 
      "number" ->
        Json.Decode.map Number (Json.Decode.field "value" Json.Decode.float) 
      "text" ->
        Json.Decode.map Text (Json.Decode.field "value" Json.Decode.string) 
      _ ->
        Json.Decode.fail <| "Unknown value type: " ++ type_
  )

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
  , randomSeed: Random.Seed
  }

type alias SerializableModel =
  { names: Dict DefinitionID TranslatableString
  , descriptions: Dict DefinitionID TranslatableString
  , nibs: Dict NibID TranslatableString
  , implementations: Dict DefinitionID Implementation
  }

exampleID = "695eec7b-3084-40e3-994b-59028c466d1e"
init : String -> Url -> Nav.Key -> (Model, Cmd Msg)
init initialJson url key = 
  case Json.Decode.decodeString decodeState initialJson of
    Ok initialState ->
      makeInitialState (log "Initial!" initialState) url key

    Err error ->
      let
        _ = log "error" error
        _ = log "state" initialJson
      in
      makeInitialState exampleState url key

exampleState =
  { names= Dict.fromList [(exampleID, makeTranslatable "Example")]
  , descriptions= Dict.fromList [(exampleID, makeTranslatable "Description of example")]
  , nibs= Dict.fromList []
  , implementations=Dict.fromList
  [ (exampleID, ConstantImplementation (Number 9.8))]
  } 

makeInitialState initialState url key = 
  ( { names = initialState.names
    , descriptions = initialState.descriptions
    , nibs = initialState.nibs
    , implementations = initialState.implementations
    , navKey = key
    , currentDefinitionID = definitionIDFromUrl url
    , randomSeed = Random.initialSeed 0
    }
  , Cmd.none)

encodeState : Model -> Json.Encode.Value
encodeState model =
  Json.Encode.object
    [ ("names", Json.Encode.dict identity encodeTranslatable model.names)
    , ("descriptions", Json.Encode.dict identity encodeTranslatable model.descriptions)
    , ("nibs", Json.Encode.dict identity encodeTranslatable model.nibs)
    , ("implementations", Json.Encode.dict identity encodeImplementation model.implementations)
    ]

decodeTranslatableDict =
  Json.Decode.dict decodeTranslatable

decodeState : Json.Decode.Decoder SerializableModel
decodeState =
  Json.Decode.map4
    (\names descriptions nibs implementations -> {names=names, descriptions=descriptions, nibs=nibs, implementations=implementations})
    ( Json.Decode.field "names" decodeTranslatableDict )
    ( Json.Decode.field "descriptions" decodeTranslatableDict )
    ( Json.Decode.field "nibs" decodeTranslatableDict )
    ( Json.Decode.field "implementations" (Json.Decode.dict decodeImplementation))
    

-- update

type Msg
  = ChangedUrl Url
  | ClickedLink Browser.UrlRequest
  | NewConstant
  | ChangeName DefinitionID String
  | ChangeConstantValue DefinitionID Value
  | NoOp
  | SaveState

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
      case request of
        Browser.Internal url ->
          ( model , Nav.pushUrl model.navKey (Url.toString url))
        Browser.External href ->
          ( model, Nav.load href )
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
    SaveState ->
      ( model
      , saveState (encodeState model)
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
  Time.every 1000 (\_ -> SaveState)

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
  on "input" (Json.Decode.map tagger targetValue)

view : Model -> Browser.Document Msg
view model =
  { title = "Nameless Programming Language"
  , body = 
    [ div [] 
      [ button [onClick NewConstant] [text "New Constant"]
      , case model.currentDefinitionID of
          Nothing ->
            div []
            [ h1 [] [text "Definitions"]
            , ul []
              (List.map (\(definitionID, translatableName) ->
                let name = getTranslatable translatableName in
                li []
                [ a [href ("#" ++ definitionID)]
                  [ text (if name == "" then "(nameless)" else name)] ]
              ) (Dict.toList model.names))
            ]
          
          Just definitionID ->
            case Dict.get definitionID model.implementations of
              Nothing ->
                div [] [text "404"]
              Just implementation ->
                case implementation of
                  ConstantImplementation constantValue ->
                    viewConstant model definitionID constantValue
                  ExternalImplementation ->
                    div [] [text "External"]
                  GraphImplementation _ ->
                    div [] [text "Graph"]
      ]
    ]
  }

viewConstant model definitionID constantValue =
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
