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
import Html.Attributes exposing (style, class)
import Html.Events exposing (on, onMouseUp, onClick)
import Html.App

import BelowTheLine.Data exposing (..)
import BelowTheLine.SenateBallot as SenateBallot

-- App

type Msg
    = LoadCandidates (List Candidate)
    | LoadFailed Http.Error
    | SelectDivision String
    | ChangeView BallotView
    | Moving (MoveItem Candidate)

type alias Model =
    { candidates : Maybe (List Candidate)
    , ballotCandidates : Maybe (List Candidate)
    , division : Maybe String
    , ballotView : BallotView
    , moving : MoveItem Candidate
    , error : Maybe String
    }

type MoveItem a = MoveItem a | MovingItem a (Float, Float) | MovedToItem a | NoItem
type BallotView = OrderBallot | ViewBallot

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
    , ballotCandidates = Nothing
    , division = Nothing
    , ballotView = OrderBallot
    , moving = NoItem
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
                    , ballotCandidates = Maybe.map (flip ballotCandidates <| candidates) model.division
                    }
                LoadFailed error ->
                    { model
                    | error = Just <| toString error
                    }
                SelectDivision division ->
                    { model
                    | division = Just division
                    , ballotCandidates = Maybe.map (ballotCandidates division) model.candidates
                    }
                ChangeView view ->
                    { model
                    | ballotView = view
                    }
                Moving movement ->
                    case movement of
                        MovedToItem candidate ->
                            case model.moving of
                                MoveItem candidate' ->
                                    { model
                                    | moving = NoItem
                                    , ballotCandidates = Maybe.map (insertBefore candidate candidate') model.ballotCandidates
                                    }
                                MovingItem candidate' _ ->
                                    { model
                                    | moving = NoItem
                                    , ballotCandidates = Maybe.map (insertBefore candidate candidate') model.ballotCandidates
                                    }
                                _ ->
                                    model
                        movement ->
                            { model
                            | moving = movement
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
        MoveItem candidate ->
            Sub.batch
                [ Mouse.moves (\pos -> Moving <| MovingItem candidate (toFloat pos.x, toFloat pos.y))
                , Mouse.ups (\pos -> Moving <| NoItem)
                ]
        MovingItem candidate _ ->
            Sub.batch
                [ Mouse.moves (\pos -> Moving <| MovingItem candidate (toFloat pos.x, toFloat pos.y))
                , Mouse.ups (\pos -> Moving <| NoItem)
                ]
        _ ->
            Sub.none

-- View

view : Model -> Html Msg
view model =
    case model.candidates of
        Just candidates ->
            Html.div []
                [ ballotSelection model candidates
                , case (model.division, model.ballotCandidates) of
                    (Just division, Just ballotCandidates) ->
                        case model.ballotView of
                            OrderBallot ->
                                candidatesView
                                model
                                ballotCandidates
                            ViewBallot ->
                                ballotView
                                division
                                candidates
                                ballotCandidates
                    _ ->
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
                [Html.text "Select a state"]

        divisionOptions =
            defaultOption ::
            List.map
                divisionOption
                (divisions candidates)

        divisionSelect =
            Html.select
                [onChange SelectDivision]
                divisionOptions

        viewToggle =
            case model.ballotView of
                OrderBallot ->
                    Html.button
                    [onClick <| ChangeView ViewBallot]
                    [Html.text "View your ballot paper"]
                ViewBallot ->
                    Html.button
                    [onClick <| ChangeView OrderBallot]
                    [Html.text "Change your preference order"]
    in
        Html.div []
            [ Html.text "Select your state"
            , Html.text " "
            , divisionSelect
            , case model.division of
                Just _ -> viewToggle
                Nothing -> Html.text ""
            ]

candidatesView : Model -> List Candidate -> Html Msg
candidatesView model candidates =
    let
        name candidate =
            [ Html.text candidate.givenName
            , Html.text " "
            , Html.text candidate.surname
            ]

        party candidate =
            [ Html.text candidate.party
            ]

        view candidate =
            [ Html.span [class "name"] <| name candidate
            , Html.text " "
            , Html.span [class "party"] <| party candidate
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
                , onMouseDown (Moving <| MoveItem candidate)
                , onMouseUp (Moving <| MovedToItem candidate)
                , style <| case model.moving of
                    NoItem -> grabCursor []
                    moving -> grabbingCursor []
                ]
                <| view candidate

        moving =
            case model.moving of
                MovingItem candidate (x,y) ->
                    Html.div
                        [style
                            [ ("position", "absolute")
                            , ("left", toString x ++ "px")
                            , ("top", toString y ++ "px")
                            , ("pointer-events", "none")
                            ]
                        ]
                        <| view candidate
                _ -> Html.text ""
    in
        Html.div []
            [ items
            , moving
            ]

ballotView : String -> List Candidate -> List Candidate -> Html Msg
ballotView division candidates ballotCandidates =
    SenateBallot.ballotView
        (ticketCandidates division candidates)
        ballotCandidates

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

onMouseDown : msg -> Html.Attribute msg
onMouseDown msg =
    Html.Events.onWithOptions
        "mousedown"
        {stopPropagation = False, preventDefault = True}
        (Json.succeed msg)
