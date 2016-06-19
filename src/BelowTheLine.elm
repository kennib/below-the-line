module BelowTheLine exposing
    (..)

import List.Extra as List

import String
import Char
import Json.Decode as Json exposing ((:=))

import Mouse

import Task
import Http
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onMouseUp)
import Html.App

import BelowTheLine.Data exposing (..)

-- App

type Msg
    = LoadCandidates (List Candidate)
    | LoadFailed Http.Error
    | SelectDivision String
    | Moving Candidate (Float, Float)
    | MovedTo Candidate
    | MovingStopped

type alias Model =
    { candidates : Maybe (List Candidate)
    , division : Maybe String
    , moving : Maybe (Candidate, (Float, Float))
    , error : Maybe String
    }

main =
    Html.App.program
        { init = (initModel, fetchCandidates)
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

fetchCandidates : Cmd Msg
fetchCandidates =
    Task.perform LoadFailed LoadCandidates
        <| fetchData "/candidates.json"

-- Model

initModel : Model
initModel =
    { candidates = Nothing
    , division = Nothing
    , moving = Nothing
    , error = Nothing
    }

-- Update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        model' =
            case msg of
                LoadCandidates candidates ->
                    { model
                    | candidates = Just candidates
                    }
                LoadFailed error ->
                    { model
                    | error = Just <| toString error
                    }
                SelectDivision division ->
                    { model
                    | division = Just division
                    }
                Moving candidate pos ->
                    { model
                    | moving = Just (candidate, pos)
                    }
                MovedTo candidate ->
                    case model.moving of
                        Just (candidate', pos) ->
                            { model
                            | moving = Nothing
                            , candidates = Maybe.map (insertBefore candidate candidate') model.candidates
                            }
                        Nothing ->
                            model
                MovingStopped ->
                    { model
                    | moving = Nothing
                    }
    in
        (model', Cmd.none)

insertBefore : a -> a -> List a -> List a
insertBefore before item items =
    let
        items' = List.filter ((/=) item) items
        index = List.elemIndex before items'
    in
        case index of
            Just index ->
                let
                    (head, tail) = List.splitAt index items'
                in
                    head ++ [item] ++ tail
            Nothing ->
                items


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.moving of
        Just (candidate, _) ->
            Sub.batch
                [ Mouse.moves (\pos -> Moving candidate (toFloat pos.x, toFloat pos.y))
                , Mouse.ups (\pos -> MovingStopped)
                ]
        Nothing ->
            Sub.none

-- View

view : Model -> Html Msg
view model =
    case model.candidates of
        Just candidates ->
            Html.div []
                [ ballotSelection model candidates
                , case model.division of
                    Just division ->
                        candidatesView
                        model
                        (ballotCandidates division candidates)
                    Nothing ->
                        Html.text ""
                ]
        Nothing ->
            Html.div []
            [case model.error of
                Just error -> Html.text error
                Nothing -> Html.text "Loading candidates"
            ]

ballotSelection : Model -> List Candidate -> Html Msg
ballotSelection model candidates =
    let
        divisionOption division = Html.option [] [Html.text division]
        defaultOption =
            Html.option
                [Html.Attributes.disabled True, Html.Attributes.selected True]
                [Html.text "Select a state or electorate"]

        divisionOptions =
            defaultOption ::
            List.map
                divisionOption
                (divisions candidates)

        divisionSelect =
            Html.select
                [onChange SelectDivision]
                divisionOptions
    in
        Html.div []
            [ Html.text "Select your electorate or state"
            , Html.text " "
            , divisionSelect
            ]


candidatesView : Model -> List Candidate -> Html Msg
candidatesView model candidates =
    let
        name candidate =
            [ Html.text candidate.givenName
            , Html.text " "
            , Html.text candidate.surname
            ]

        items =
            Html.ol
                [style <| unselectable []]
                <| List.indexedMap
                    item
                    candidates

        item index candidate =
            Html.li
                [ Html.Attributes.id (toString index)
                , onMouseDown (Moving candidate)
                , onMouseUp (MovedTo candidate)
                , style <| case model.moving of
                    Just moving -> grabbingCursor []
                    Nothing -> grabCursor []
                ]
                <| name candidate

        moving =
            case model.moving of
                Just (candidate, (x,y)) ->
                    Html.div
                        [style
                            [ ("position", "absolute")
                            , ("left", toString x ++ "px")
                            , ("top", toString y ++ "px")
                            , ("pointer-events", "none")
                            ]
                        ]
                        <| name candidate
                Nothing -> Html.text ""
    in
        Html.div []
            [ items
            , moving
            ]

unselectable : List (String, String) -> List (String, String)
unselectable style =
    style ++
    [ ("user-select", "none")
    , ("-webkit-user-select", "none")
    , ("user-drag", "none")
    , ("-webkit-user-drag", "none")
    ]

grabCursor : List (String, String) -> List (String, String)
grabCursor style =
    style ++
    [ ("cursor", "move")
    , ("cursor", "grab")
    , ("cursor", "-webkit-grab")
    ]

grabbingCursor : List (String, String) -> List (String, String)
grabbingCursor style =
    style ++
    [ ("cursor", "move")
    , ("cursor", "grabbing")
    , ("cursor", "-webkit-grabbing")
    ]

-- Events

onChange : (String -> msg) -> Html.Attribute msg
onChange msg =
    on "change" (Json.map msg (Json.at ["target", "value"] Json.string))

onMouseDown : ((Float, Float) -> msg) -> Html.Attribute msg
onMouseDown msg =
    Html.Events.onWithOptions
        "mousedown"
        {stopPropagation = False, preventDefault = True}
        (Json.map msg coords)

coords : Json.Decoder (Float, Float)
coords =
    Json.object2 (,)
        ("clientX" := Json.float)
        ("clientY" := Json.float)
