port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Debug exposing (log)
import Dict exposing (Dict)
import Dict.Any
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Http
import Json.Decode
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode
import Random exposing (Seed)
import Set exposing (Set)
import Time
import UUID exposing (UUID)
import Url exposing (Url)


port saveState : Json.Encode.Value -> Cmd msg


googleApiKey =
    "AIzaSyDyNAGC8MhTFPrUoYY4RHENX9M4ZcfEIis"



-- main


main : Program Json.Encode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }


type alias Flags =
    { state : String
    , language : String
    }



-- model
--- documentation


type alias TranslatableString =
    { sourceLanguage : Language
    , translations : Dict Language VettableString
    }


type alias Language =
    String


type alias VettableString =
    { string : String
    , vetted : Bool
    }


currentLanguage =
    "en"


makeTranslatable : Language -> String -> TranslatableString
makeTranslatable language string =
    TranslatableString language (Dict.fromList [ ( language, VettableString string True ) ])


getTranslatable : Language -> TranslatableString -> String
getTranslatable language translatable =
    case Dict.get language translatable.translations of
        Nothing ->
            ""

        Just vettableString ->
            vettableString.string


updateTranslatable : Language -> TranslatableString -> String -> TranslatableString
updateTranslatable language translatable string =
    { translatable
        | translations =
            Dict.insert language (VettableString string False) translatable.translations
    }


encodeTranslatable : TranslatableString -> Json.Encode.Value
encodeTranslatable translatable =
    Json.Encode.object
        [ ( "sourceLanguage", Json.Encode.string translatable.sourceLanguage )
        , ( "translations", Json.Encode.dict identity encodeVettable translatable.translations )
        ]


encodeVettable vettable =
    Json.Encode.object
        [ ( "string", Json.Encode.string vettable.string )
        , ( "vetted", Json.Encode.bool vettable.vetted )
        ]


decodeTranslatable : Json.Decode.Decoder TranslatableString
decodeTranslatable =
    Json.Decode.map2
        (\sourceLanguage translations -> TranslatableString sourceLanguage translations)
        (Json.Decode.field "sourceLanguage" Json.Decode.string)
        (Json.Decode.field "translations" (Json.Decode.dict decodeVettable))


decodeVettable =
    Json.Decode.map2
        (\string vetted -> VettableString string vetted)
        (Json.Decode.field "string" Json.Decode.string)
        (Json.Decode.field "vetted" Json.Decode.bool)



--- implementation


type alias DefinitionID =
    String


type alias NibID =
    String


type alias NodeID =
    String


type Value
    = Integer Int
    | Number Float
    | Text String


type Type
    = IntegerType
    | NumberType
    | TextType
    | RecordType DefinitionID
    | UnionType DefinitionID
    | FunctionPointerType DefinitionID


valueToTypeString value =
    case value of
        Integer _ ->
            "integer"

        Number _ ->
            "number"

        Text _ ->
            "text"


type ConnectionNode
    = Node NodeID
    | Graph


type ConnectionNib
    = Nib NibID
    | ValueNib


type alias NibConnection =
    { node : ConnectionNode
    , nib : ConnectionNib
    }


comparableNodeConnection : NibConnection -> ( String, String )
comparableNodeConnection value =
    ( case value.node of
        Node node_id ->
            node_id

        Graph ->
            ""
    , case value.nib of
        Nib nib_id ->
            nib_id

        ValueNib ->
            ""
    )


type Node
    = DefinedNode
        { kind : DefinedNodeType
        , definitionID : DefinitionID
        }
    | ReferenceNode


getNodeDefinitionID : Node -> Maybe DefinitionID
getNodeDefinitionID node =
    case node of
        DefinedNode { definitionID } ->
            Just definitionID

        _ ->
            Nothing


type DefinedNodeType
    = FunctionCallNode
    | ValueNode
    | FunctionPointerCallNode
    | FunctionDefinitionNode
    | ConstructorNode
    | AccessorNode



-- type alias Connection =
--     { source : NibConnection
--     , sink : NibConnection
--     }


type Implementation
    = ConstantImplementation Value
    | ExternalImplementation Interface String
    | GraphImplementation GraphImplementationRecord
    | InterfaceImplementation Interface
    | RecordTypeImplementation TypeImplementation
    | UnionTypeImplementation TypeImplementation


type alias GraphImplementationRecord =
    { connections : Connections
    , nodes : Dict NodeID Node
    }


getGraphDirectDependencies : GraphImplementationRecord -> Set DefinitionID
getGraphDirectDependencies graph_implementation =
    Set.fromList (List.filterMap getNodeDefinitionID (Dict.values graph_implementation.nodes))


getDirectDependencies : Implementation -> Set DefinitionID
getDirectDependencies implementation =
    case implementation of
        GraphImplementation record ->
            getGraphDirectDependencies record

        _ ->
            Set.empty


getFullDependencies : DefinitionID -> Implementations -> Result String (Set DefinitionID)
getFullDependencies definitionID implementations =
    getFullDependenciesWorker definitionID implementations Set.empty


getFullDependenciesWorker : DefinitionID -> Implementations -> Set DefinitionID -> Result String (Set DefinitionID)
getFullDependenciesWorker definitionID implementations visited =
    case Dict.get definitionID implementations of
        Nothing ->
            Err "Definition not found"

        Just implementation ->
            let
                directDependencies =
                    getDirectDependencies implementation

                newDependencies =
                    Set.diff directDependencies visited

                allDependencies =
                    Set.union visited directDependencies
            in
            Set.foldl
                (\childDefinitionID acc ->
                    case acc of
                        Ok set ->
                            case getFullDependenciesWorker childDefinitionID implementations set of
                                Ok items ->
                                    Ok (Set.union set items)

                                x ->
                                    x

                        x ->
                            x
                )
                (Ok allDependencies)
                newDependencies


type alias Connections =
    Dict.Any.AnyDict ( String, String ) NibConnection NibConnection


type OrderedNibConnection
    = OrderedNodeConnection { nibIndex : Int, nodeID : ContentID }
    | OrderedGraphConnection { nibIndex : Int }


type alias ContentID =
    String



-- graphContentId: Connections -> List NibID -> List NibID -> Map DefinitionID ContentID ->
-- graphContentId connections outputs inputs dependencies =
--     List.foldl ()


type alias Interface =
    { inputTypes : Dict NibID Type
    , outputTypes : Dict NibID Type
    }


type alias TypeImplementation =
    { fieldTypes : Dict NibID Type }


encodeImplementation : Implementation -> Json.Encode.Value
encodeImplementation implementation =
    case implementation of
        ConstantImplementation value ->
            encodeConstant value

        GraphImplementation _ ->
            Json.Encode.string "TODO"

        ExternalImplementation _ _ ->
            Json.Encode.string "TODO"

        InterfaceImplementation _ ->
            Json.Encode.string "TODO"

        RecordTypeImplementation _ ->
            Json.Encode.string "TODO"

        UnionTypeImplementation _ ->
            Json.Encode.string "TODO"


decodeImplementation : Json.Decode.Decoder Implementation
decodeImplementation =
    Json.Decode.field "type" Json.Decode.string
        |> Json.Decode.andThen
            (\type_ ->
                case type_ of
                    "constant" ->
                        Json.Decode.map ConstantImplementation (Json.Decode.field "value" valueDecoder)

                    _ ->
                        Json.Decode.fail <| "Unknown implementation type: " ++ type_
            )


encodeConstant : Value -> Json.Encode.Value
encodeConstant value =
    Json.Encode.object
        [ ( "type", Json.Encode.string "constant" )
        , ( "value"
          , let
                ( name, jsonValue ) =
                    case value of
                        Integer integer ->
                            ( "integer", Json.Encode.int integer )

                        Number number ->
                            ( "number", Json.Encode.float number )

                        Text text ->
                            ( "text", Json.Encode.string text )
            in
            Json.Encode.object
                [ ( "type", Json.Encode.string name )
                , ( "value", jsonValue )
                ]
          )
        ]


valueDecoder : Json.Decode.Decoder Value
valueDecoder =
    Json.Decode.field "type" Json.Decode.string
        |> Json.Decode.andThen
            (\type_ ->
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



--- init


type alias Implementations =
    Dict DefinitionID Implementation


type alias Model =
    { names : Dict DefinitionID TranslatableString
    , descriptions : Dict DefinitionID TranslatableString
    , nibs : Dict NibID TranslatableString
    , implementations : Implementations
    , navKey : Nav.Key
    , currentDefinitionID : Maybe DefinitionID
    , randomSeed : Random.Seed
    , languageChoices : List ( String, String )
    , failedLoad : Bool
    , language : Language
    }


type alias SerializableModel =
    { names : Dict DefinitionID TranslatableString
    , descriptions : Dict DefinitionID TranslatableString
    , nibs : Dict NibID TranslatableString
    , implementations : Dict DefinitionID Implementation
    }


exampleID =
    "695eec7b-3084-40e3-994b-59028c466d1e"


helloID =
    "91a72968-c9b4-4ebd-bcec-7c0280989f00"


worldID =
    "e3723bd3-9142-4171-a3cb-4e7a7da57564"


concatenateID =
    "69e342c5-8c55-4ae6-a3b0-1080c4ac8270"


concatenateLeftInputID =
    "8218e386-f0eb-4e16-9230-0e560b44d5f4"


concatenateRightInputID =
    "5ab9e3c2-c7df-4345-9bc2-bd0e03f47a4e"


concatenateOutputID =
    "90a3cbf4-a320-4396-968d-f8847c60a8b8"


helloWorldConcatenateNodeID =
    "4b9a870e-2dae-4504-8920-141b24b01e71"


helloWorldID =
    "41ffb3fd-9a00-4e9f-961d-85430a689160"


helloWorldHelloNodeID =
    "888a06a1-ee06-497d-a393-25800418308c"


helloWorldWorldNodeID =
    "e2fc3dbf-b7bf-4965-95d0-1ab10e408843"


helloWorldOutputNibID =
    "5967b40d-1781-4590-bc12-5295a51c0e6e"


init : Json.Encode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flagsJson url key =
    case Json.Decode.decodeValue decodeFlags flagsJson of
        Ok { language, state } ->
            if state == "" then
                makeInitialState exampleState language url key False

            else
                case Json.Decode.decodeString decodeState state of
                    Ok initialState ->
                        makeInitialState (log "Initial!" initialState) language url key False

                    Err error ->
                        let
                            _ =
                                log "Failed to parse the state: " error

                            _ =
                                log "State was: " flagsJson
                        in
                        makeInitialState exampleState language url key True

        Err error ->
            let
                _ =
                    log "Bad flags passed to init: " error
            in
            makeInitialState exampleState "en" url key True


exampleState : SerializableModel
exampleState =
    { names =
        Dict.fromList
            [ ( exampleID, makeTranslatable "en" "Example" )
            , ( helloID, makeTranslatable "en" "Hello" )
            , ( worldID, makeTranslatable "en" "World" )
            , ( concatenateID, makeTranslatable "en" "Concatenate" )
            , ( helloWorldID, makeTranslatable "en" "Hello World" )
            ]
    , descriptions =
        Dict.fromList
            [ ( exampleID, makeTranslatable "en" "Description of example" )
            , ( helloWorldID, makeTranslatable "en" "Example of concatenating two strings." )
            ]
    , nibs =
        Dict.fromList
            [ ( helloWorldOutputNibID, makeTranslatable "en" "" )
            ]
    , implementations =
        Dict.fromList
            [ ( exampleID, ConstantImplementation (Number 9.8) )
            , ( helloID, ConstantImplementation (Text "hello") )
            , ( worldID, ConstantImplementation (Text "world") )
            , ( concatenateID
              , ExternalImplementation
                    { inputTypes = Dict.fromList [ ( concatenateLeftInputID, TextType ), ( concatenateRightInputID, TextType ) ]
                    , outputTypes = Dict.fromList [ ( concatenateOutputID, TextType ) ]
                    }
                    "concatenate"
              )
            , ( helloWorldID
              , GraphImplementation
                    { nodes =
                        Dict.fromList
                            [ ( helloWorldConcatenateNodeID, DefinedNode { kind = FunctionCallNode, definitionID = concatenateID } )
                            , ( helloWorldHelloNodeID, DefinedNode { kind = ValueNode, definitionID = helloID } )
                            , ( helloWorldWorldNodeID, DefinedNode { kind = ValueNode, definitionID = worldID } )
                            ]
                    , connections =
                        Dict.Any.fromList comparableNodeConnection
                            [ ( { node = Node helloWorldConcatenateNodeID, nib = Nib concatenateLeftInputID }
                              , { node = Node helloWorldHelloNodeID, nib = ValueNib }
                              )
                            , ( { node = Node helloWorldConcatenateNodeID, nib = Nib concatenateRightInputID }
                              , { node = Node helloWorldWorldNodeID, nib = ValueNib }
                              )
                            , ( { node = Graph, nib = Nib helloWorldOutputNibID }
                              , { node = Node helloWorldConcatenateNodeID, nib = Nib concatenateOutputID }
                              )
                            ]
                    }
              )
            ]
    }


makeInitialState : SerializableModel -> Language -> Url -> Nav.Key -> Bool -> ( Model, Cmd Msg )
makeInitialState initialState language url key failedLoad =
    ( { names = initialState.names
      , descriptions = initialState.descriptions
      , nibs = initialState.nibs
      , implementations = initialState.implementations
      , navKey = key
      , currentDefinitionID = definitionIDFromUrl url
      , randomSeed = Random.initialSeed 0
      , languageChoices = []
      , failedLoad = failedLoad
      , language = language
      }
    , Http.post
        { url = "https://translation.googleapis.com/language/translate/v2/languages?key=" ++ googleApiKey
        , body = Http.jsonBody (Json.Encode.object [ ( "target", Json.Encode.string "en" ) ])
        , expect = Http.expectJson GotLanguageChoices languageChoiceDecoder
        }
    )



-- curl -X POST \
--      -H "Content-Type: application/json; charset=utf-8" \
--      --data "{
--   'target': 'en'
-- }" "https://translation.googleapis.com/language/translate/v2/languages?key=AIzaSyDyNAGC8MhTFPrUoYY4RHENX9M4ZcfEIis"


encodeState : Model -> Json.Encode.Value
encodeState model =
    Json.Encode.object
        [ ( "names", Json.Encode.dict identity encodeTranslatable model.names )
        , ( "descriptions", Json.Encode.dict identity encodeTranslatable model.descriptions )
        , ( "nibs", Json.Encode.dict identity encodeTranslatable model.nibs )
        , ( "implementations", Json.Encode.dict identity encodeImplementation model.implementations )
        ]


decodeTranslatableDict =
    Json.Decode.dict decodeTranslatable


decodeState : Json.Decode.Decoder SerializableModel
decodeState =
    Json.Decode.map4
        (\names descriptions nibs implementations -> { names = names, descriptions = descriptions, nibs = nibs, implementations = implementations })
        (Json.Decode.field "names" decodeTranslatableDict)
        (Json.Decode.field "descriptions" decodeTranslatableDict)
        (Json.Decode.field "nibs" decodeTranslatableDict)
        (Json.Decode.field "implementations" (Json.Decode.dict decodeImplementation))


decodeFlags : Json.Decode.Decoder Flags
decodeFlags =
    Json.Decode.map2
        (\language state -> { language = language, state = state })
        (Json.Decode.field "language" Json.Decode.string)
        (Json.Decode.field "state" Json.Decode.string)



-- update


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | NewConstant
    | ChangeName DefinitionID String
    | ChangeConstantValue DefinitionID Value
    | NoOp
    | SaveState
    | GotLanguageChoices (Result Http.Error (List ( String, String )))
    | ChangeLanguage Language


languageChoiceDecoder =
    Json.Decode.field "data"
        (Json.Decode.field "languages"
            (Json.Decode.list
                (Json.Decode.map2
                    (\code name -> ( code, name ))
                    (Json.Decode.field "language" Json.Decode.string)
                    (Json.Decode.field "name" Json.Decode.string)
                )
            )
        )


initialSeed =
    Random.initialSeed 12345


isUuid string =
    case UUID.fromString string of
        Ok _ ->
            True

        Err _ ->
            False


definitionIDFromUrl : Url -> Maybe DefinitionID
definitionIDFromUrl url =
    case url.fragment of
        Nothing ->
            Nothing

        Just fragment ->
            if isUuid fragment then
                Just fragment

            else
                Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
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
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        NewConstant ->
            let
                ( uuid, newSeed ) =
                    makeUuid initialSeed
            in
            ( { model
                | names =
                    Dict.insert uuid (makeTranslatable model.language "") model.names
                , implementations =
                    Dict.insert uuid (ConstantImplementation (Text "")) model.implementations
              }
            , Nav.pushUrl model.navKey ("#" ++ uuid)
            )

        ChangeName uuid newName ->
            ( { model
                | names =
                    Dict.update uuid
                        (\maybeName ->
                            case maybeName of
                                Nothing ->
                                    Just (makeTranslatable model.language newName)

                                Just name ->
                                    Just (updateTranslatable model.language name newName)
                        )
                        model.names
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

        GotLanguageChoices result ->
            case result of
                Ok choices ->
                    ( { model | languageChoices = choices }
                    , Cmd.none
                    )

                Err error ->
                    let
                        _ =
                            log "Error loading language choices: " error
                    in
                    ( model, Cmd.none )

        ChangeLanguage language ->
            ( { model | language = language }
            , Cmd.none
            )



-- TODO: fetch translations


makeUuid seed =
    let
        ( uuid, newSeed ) =
            Random.step UUID.generator seed
    in
    ( UUID.canonical uuid, newSeed )



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
            getTranslatable model.language translatableString



--view


nameView model definitionID =
    input
        [ onInput (\text -> ChangeName definitionID text)
        , value (getName model definitionID)
        ]
        []


onChange : (String -> msg) -> Attribute msg
onChange tagger =
    on "input" (Json.Decode.map tagger targetValue)


view : Model -> Browser.Document Msg
view model =
    { title = "Nameless Programming Language"
    , body =
        [ div []
            [ button [ onClick NewConstant ] [ text "New Constant" ]
            , case model.currentDefinitionID of
                Nothing ->
                    div []
                        [ h1 [] [ text "Definitions" ]
                        , ul []
                            (List.map
                                (\( definitionID, translatableName ) ->
                                    let
                                        name =
                                            getTranslatable model.language translatableName
                                    in
                                    li []
                                        [ a [ href ("#" ++ definitionID) ]
                                            [ text
                                                (if name == "" then
                                                    "(nameless)"

                                                 else
                                                    name
                                                )
                                            ]
                                        ]
                                )
                                (Dict.toList model.names)
                            )
                        ]

                Just definitionID ->
                    case Dict.get definitionID model.implementations of
                        Nothing ->
                            div [] [ text "404" ]

                        Just implementation ->
                            case implementation of
                                ConstantImplementation constantValue ->
                                    viewConstant model definitionID constantValue

                                ExternalImplementation _ _ ->
                                    div [] [ text "External" ]

                                GraphImplementation _ ->
                                    div [] [ text "Graph" ]

                                InterfaceImplementation _ ->
                                    div [] [ text "Interface" ]

                                RecordTypeImplementation _ ->
                                    div [] [ text "Record" ]

                                UnionTypeImplementation _ ->
                                    div [] [ text "Union" ]
            ]
        ]
    }


viewConstant model definitionID constantValue =
    let
        typeName =
            valueToTypeString constantValue
    in
    div []
        [ text ("Editing a constant: " ++ valueAsText constantValue)
        , viewLanguageSelector model
        , nameView model definitionID
        , select
            [ onChange
                (\type_ ->
                    ChangeConstantValue definitionID
                        (case type_ of
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

                            _ ->
                                constantValue
                        )
                )
            ]
            [ viewOption typeName "integer" "Integer"
            , viewOption typeName "number" "Number"
            , viewOption typeName "text" "Text"
            ]
        , case constantValue of
            Text string ->
                textarea
                    [ onChange (\value -> ChangeConstantValue definitionID (Text value)) ]
                    [ text string ]

            Number number ->
                input
                    [ type_ "number"
                    , step "any"
                    , value (String.fromFloat number)
                    , onChange
                        (\value ->
                            case String.toFloat value of
                                Nothing ->
                                    NoOp

                                Just newNumber ->
                                    ChangeConstantValue definitionID (Number newNumber)
                        )
                    ]
                    []

            Integer integer ->
                input
                    [ type_ "number"
                    , value (String.fromInt integer)
                    , onChange
                        (\value ->
                            case String.toInt value of
                                Nothing ->
                                    NoOp

                                Just newInteger ->
                                    ChangeConstantValue definitionID (Integer newInteger)
                        )
                    ]
                    []
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
    option [ value name, selected (selectedName == name) ] [ text display ]


viewLanguageSelector : Model -> Html Msg
viewLanguageSelector model =
    select [ onChange (\value -> ChangeLanguage value) ]
        (List.map (\( code, name ) -> viewOption model.language code name) model.languageChoices)
