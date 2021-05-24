module Main exposing (main)

import Array exposing (Array)
import Dict exposing (Dict)
import Browser
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode as Json

{-| A turing machine simulator with the following features.

- the head must move at each step.
- the tape is indefinitely extensible to the both side.
-}

main : Program () Model Msg
main =
  Browser.element
    { init = \_ -> ( initModel, Cmd.none )
    , update = update
    , view = view
    , subscriptions = subscriptions
    }



-- MODEL --

type alias Model =
  { tape : Tape
  , head : Int
  , state : String
  , delta : Dict ( State, Symbol ) ( Symbol, Direction, State )
  , tapeStr : String
  , editor : Editor
  }

type alias Editor =
  { state : State
  , symbol : Symbol
  , write : Symbol
  , move : Direction
  , next : State
  }

type alias Tape = Dict Int Symbol

type alias State = String

type alias Symbol = Char
symbols =
  [ blank
  , '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
  , 'A', 'B', 'C', 'D', 'E', 'F'
  ]

type Direction
  = Left
  | Right

blank = ' '
symbolToString = String.fromChar

initModel =
  { tape = Dict.empty
  , head = 0
  , state = "init"
  , delta = Dict.empty
  , tapeStr = ""
  , editor = emptyRule
  }

emptyRule =
  { state = ""
  , symbol = blank
  , write = blank
  , move = Left
  , next = ""
  }



-- UPDATE --

type Msg
  = Nop
  | TapeChanged String
  | Reset
  | Step
  | EditorUpdate Editor
  | InsertRule

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Nop ->
      ( model, Cmd.none )

    TapeChanged str ->
      let
        newModel =
          { model | tapeStr = str }
      in
        ( newModel, Cmd.none )

    Reset ->
      let
        charList =
          model.tapeStr
            |> String.toList

        tape =
          if charList |> List.all (\c -> List.member c symbols)
          then
            charList
              |> List.indexedMap Tuple.pair
              |> Dict.fromList
          else
            Dict.empty

        newModel =
          { model
          | tape = tape
          , head = 0
          , state = "init"
          }
      in
        ( newModel, Cmd.none )

    Step ->
      let
        read =
          readTape model.head model.tape

        ( state, tape, head ) =
          model.delta
            |> Dict.get ( model.state, read )
            |> Maybe.map
              (\( sy, di, st ) ->
                ( st
                , writeTape sy model.head model.tape
                , case di of
                    Left -> model.head - 1
                    Right -> model.head + 1
                )
              )
            |> Maybe.withDefault ( "reject", model.tape, model.head )

        newModel =
          { model
          | tape = tape
          , head = head
          , state = state
          }
      in
        ( newModel, Cmd.none )

    EditorUpdate new ->
      let
        newModel =
          { model | editor = new }
      in
        ( newModel, Cmd.none )

    InsertRule ->
      let
        { state, symbol, write, move, next } =
          model.editor

        newDelta =
          model.delta
            |> Dict.insert ( state, symbol ) ( write, move, next )

        newModel =
          { model | delta = newDelta }
      in
        ( newModel, Cmd.none )



-- VIEW --

view : Model -> Html Msg
view model =
  let
    machine =
      viewMachine model

    controler =
      viewControler

    ruleEditor =
      viewEditor model.editor

    transitionTable =
      viewDelta model.delta
  in
    Html.div [ Attrs.id "elm-area" ]
      [ machine
      , controler
      , ruleEditor
      , transitionTable
      ]

viewMachine : Model -> Html msg
viewMachine { tape, head, state } =
  Html.div [ Attrs.class "tm" ]
    [ Html.div [ Attrs.class "state"] [ Html.text state ]
    , Html.div [ Attrs.class "header"] [ Html.text "▼" ]
    , viewTape head tape
    ]

viewControler : Html Msg
viewControler =
  Html.div []
    [ Html.input [ Attrs.type_ "text", onChange TapeChanged ] []
    , Html.button [ Events.onClick Reset ] [ Html.text "Reset" ]
    , Html.button [ Events.onClick Step ] [ Html.text "Step" ]
    ]

viewTape : Int -> Tape -> Html msg
viewTape center tape =
  let
    viewDistance = 7
    leftEnd = center - viewDistance
    rightEnd = center + viewDistance

    data =
      List.range leftEnd rightEnd
        |> List.map
          (\idx ->
            let
              symbol = readTape idx tape
              string = symbolToString symbol
            in
              Html.td [] [ Html.text string ]
          )
        |> Html.tr []
  in
    Html.table [ Attrs.class "tape" ]
      [ Html.tbody []
        [ data ]
      ]

viewEditor : Editor -> Html Msg
viewEditor editor =
  let
    option str =
      Html.option [ Attrs.value str ]
        [ Html.text str ]

    symbolSelector msg =
      symbols
        |> List.map (option << symbolToString)
        |> Html.select [ onChange msg ]
  in
    Html.div [ Attrs.class "editor" ]
      [ Html.text "State: "
      , Html.input
        [ Attrs.type_ "text"
        , onChange <| EditorUpdate << editorState editor
        ]
        []
      , Html.text ", Symbol: "
      , symbolSelector <| EditorUpdate << editorSymbol editor
      , Html.text ", Write: "
      , symbolSelector <| EditorUpdate << editorWrite editor
      , Html.text ", Move: "
      , Html.select
        [ onChange <| EditorUpdate << editorMove editor
        ]
        [ option "L"
        , option "R"
        ]
      , Html.text ", Next:"
      , Html.input
        [ Attrs.type_ "text"
        , onChange <| EditorUpdate << editorNext editor
        ]
        []
      , Html.button
        [ Events.onClick InsertRule ]
        [ Html.text "add" ]
      ]

onChange : (String -> msg) -> Html.Attribute msg
onChange handler =
    Events.on "change" (Json.map handler Events.targetValue)

viewDelta : Dict ( State, Symbol ) ( Symbol, Direction, State ) -> Html msg
viewDelta delta =
  let
    header =
      [ "state"
      , "symbol"
      , "write"
      , "move"
      , "next"
      ]
        |> List.map (\h -> Html.th [] [ Html.text h ])
        |> Html.tr []

    body =
      delta
        |> Dict.toList
        |> List.map viewDeltaElement
        |> List.map (Html.tr [])

  in
    Html.table [ Attrs.class "transition" ]
      [ Html.thead [] [ Html.text "Transition Table"]
      , Html.tbody [] (header :: body) ]

viewDeltaElement : (( State, Symbol ), ( Symbol, Direction, State )) -> List (Html msg)
viewDeltaElement (( state, symbol ), ( write, move, next )) =
  [ state
  , symbol |> symbolToString
  , write |> symbolToString
  , case move of
      Left -> "L"
      Right -> "R"
  , next
  ]
    |> List.map
      (\str ->
        Html.td []
          [ Html.text str ]
      )



-- SUBSCRIPTIONS --

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- TAPE --

readTape : Int -> Tape -> Symbol
readTape idx tape =
  tape
    |> Dict.get idx
    |> Maybe.withDefault blank

writeTape : Symbol -> Int -> Tape -> Tape
writeTape symbol idx tape =
  if symbol == blank
  then tape |> Dict.remove idx
  else tape |> Dict.insert idx symbol



-- EDITOR --

editorState : Editor -> String -> Editor
editorState editor str =
  { editor | state = str }

editorSymbol : Editor -> String -> Editor
editorSymbol editor str =
  case str |> String.toList |> List.head of
    Just char ->
      if symbols |> List.member char
      then { editor | symbol = char }
      else editor

    _ -> editor

editorWrite : Editor -> String -> Editor
editorWrite editor str =
  case str |> String.toList |> List.head of
    Just char ->
      if symbols |> List.member char
      then { editor | write = char }
      else editor

    _ -> editor

editorMove : Editor -> String -> Editor
editorMove editor str =
  case str of
    "L" -> { editor | move = Left }
    "R" -> { editor | move = Right }
    _ -> editor

editorNext : Editor -> String -> Editor
editorNext editor str =
  { editor | next = str }
