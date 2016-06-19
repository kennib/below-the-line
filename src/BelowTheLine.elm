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

import BelowTheLine.Data exposing (Candidate, House(..), fetchData, ballotCandidates)

-- App

type Msg
    = LoadCandidates (List Candidate)
    | LoadFailed Http.Error
    | Moving Candidate (Float, Float)
    | MovedTo Candidate

type alias Model =
    { candidates : Maybe (List Candidate)
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
            Mouse.moves (\pos -> Moving candidate (toFloat pos.x, toFloat pos.y))
        Nothing ->
            Sub.none

-- View

view : Model -> Html Msg
view model =
    case model.candidates of
        Just candidates ->
            candidatesView model candidates
        Nothing ->
            Html.div []
            [case model.error of
                Just error -> Html.text error
                Nothing -> Html.text "Loading candidates"
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
            Html.ol []
                <| List.indexedMap
                    item
                    candidates

        item index candidate =
            Html.li
                [ Html.Attributes.id (toString index)
                , onMouseDown (Moving candidate)
                , onMouseUp (MovedTo candidate)
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

-- Events

onMouseDown : ((Float, Float) -> msg) -> Html.Attribute msg
onMouseDown msg =
    on "mousedown" (Json.map msg coords)

coords : Json.Decoder (Float, Float)
coords =
    Json.object2 (,)
        ("clientX" := Json.float)
        ("clientY" := Json.float)
