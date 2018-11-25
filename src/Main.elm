import Browser
import Browser.Navigation as Nav
import Url exposing (Url)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Dict exposing (Dict)
import Random exposing (Seed, initialSeed, step)
import UUID exposing (UUID)
import Debug exposing (log)

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
  | Float Float
  | String String

typeNames = 
  [ "Integer"
  , "Floating Point Number"
  ]

type Implementation
  = ConstantImplementation String Value
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

type alias Model =
  { names: Dict DefinitionID TranslatableString
  , descriptions: Dict DefinitionID TranslatableString
  , nibs: Dict NibID TranslatableString
  , implementation: Dict DefinitionID Implementation
  , navKey: Nav.Key
  , currentDefinitionID: Maybe DefinitionID
  }

init : () -> Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key = 
  (
    { names= Dict.fromList [(exampleID, makeTranslatable "Example")]
    , descriptions= Dict.fromList [(exampleID, makeTranslatable "Description of example")]
    , nibs= Dict.fromList []
    , implementation=Dict.fromList [(exampleID, ConstantImplementation "Gravity" (Float 9.8))]
    , navKey = key
    , currentDefinitionID = Nothing
    }
  , Cmd.none)

-- update

type Msg
  = ChangedUrl Url
  | ClickedLink Browser.UrlRequest
  | NewConstant
  | ChangeName DefinitionID String

initialSeed = Random.initialSeed 12345

isUuid maybeString =
  case maybeString of
    Nothing -> False
    Just string ->
      case UUID.fromString string of
        Ok _ -> True
        Err _ -> False

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    ChangedUrl url ->
      ({model | currentDefinitionID =
        if (isUuid url.fragment) then Just url.path else Nothing}, Cmd.none)
    ClickedLink request ->
      (model, Cmd.none)
    NewConstant ->
      let (uuid, newSeed) = makeUuid initialSeed in
      ( editNames model (\names ->
          Dict.insert uuid (makeTranslatable "") names)
      , Nav.pushUrl model.navKey ("#" ++ uuid))
    ChangeName uuid newName ->
      ( editNames model (\names -> 
          Dict.update uuid (\maybeName ->
            case maybeName of
              Nothing ->
                Just (makeTranslatable newName)
              Just name ->
                Just (updateTranslatable name newName)
          ) names
        )
      , Cmd.none
      )

-- addConstant : Model -> UUID -> Model
-- addConstant model uuid =
--   { model |
--     names = 
--   }

makeUuid seed = 
  let (uuid, newSeed) = Random.step UUID.generator seed in (UUID.canonical uuid, newSeed)

editNames model edit =
  {
    model | names = edit model.names
  }

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
            div []
            [ text "Editing a constant: "
            , nameView model definitionID

            ]
      ]
    ]
  }
