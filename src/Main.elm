port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Debug exposing (log)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Http
import Json.Decode
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode
import Random exposing (Seed)
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


type Implementation
    = ConstantImplementation Value
    | ExternalImplementation Interface String
    | GraphImplementation
        { connections : Dict NibConnection NibConnection
        , nodes : Dict NodeID Node
        }
    | InterfaceImplementation Interface
    | RecordTypeImplementation TypeImplementation
    | UnionTypeImplementation TypeImplementation


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


type NibConnection
    = NodeConnection { nibID : NibID, nodeID : NodeID }
    | GraphConnection { nibID : NibID }


type Node
    = CallNode DefinitionID
    | ValueNode DefinitionID



--- extra


type alias Connection =
    { source : NibConnection
    , sink : NibConnection
    }



--- init


type alias Model =
    { names : Dict DefinitionID TranslatableString
    , descriptions : Dict DefinitionID TranslatableString
    , nibs : Dict NibID TranslatableString
    , implementations : Dict DefinitionID Implementation
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
    { names = Dict.fromList [ ( exampleID, makeTranslatable "en" "Example" ) ]
    , descriptions = Dict.fromList [ ( exampleID, makeTranslatable "en" "Description of example" ) ]
    , nibs = Dict.fromList []
    , implementations =
        Dict.fromList
            [ ( exampleID, ConstantImplementation (Number 9.8) ) ]
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
