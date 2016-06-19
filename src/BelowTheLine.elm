module BelowTheLine exposing
    (..)

import List.Extra as List

import String
import Char
import Json.Decode as Json exposing ((:=))

import Mouse

import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onMouseUp)
import Html.App

-- App

type Msg
    = Moving String (Float, Float)
    | MovedTo String

type alias Model =
    { candidates : List String
    , moving : Maybe (String, (Float, Float))
    }

main =
    Html.App.program
        { init = (model, Cmd.none)
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

-- Model

model : Model
model =
    { candidates = candidates
    , moving = Nothing
    }

candidates : List String
candidates =
    List.map
        (\i -> "Candidate " ++ String.fromChar (Char.fromCode i))
        [65..90]

-- Update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        model' =
            case msg of
                Moving candidate pos ->
                    { model
                    | moving = Just (candidate, pos)
                    }
                MovedTo candidate ->
                    case model.moving of
                        Just (candidate', pos) ->
                            { model
                            | moving = Nothing
                            , candidates = insertBefore candidate candidate' model.candidates
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
    let
        items =
            Html.ol []
                <| List.indexedMap
                    item
                    model.candidates

        item index candidate =
            Html.li
                [ Html.Attributes.id (toString index)
                , onMouseDown (Moving candidate)
                , onMouseUp (MovedTo candidate)
                ]
                [Html.text candidate]

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
                        [Html.text candidate]
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
