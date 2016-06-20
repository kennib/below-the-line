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
    | TogglePreference Candidate
    | Moving (MoveItem Candidate)

type alias Model =
    { candidates : Maybe (List Candidate)
    , ballotCandidates : Maybe (List Candidate)
    , preferences : List Candidate
    , division : Maybe String
    , ballotView : BallotView
    , moving : MoveItem Candidate
    , error : Maybe String
    }

type MoveItem a = MoveItem a | MovingItem a (Float, Float) | MovedToItem a | MovedToEnd | NoItem
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
    , preferences = []
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
                TogglePreference candidate ->
                    { model
                    | preferences = toggle candidate model.preferences
                    }
                Moving movement ->
                    case movement of
                        MovedToEnd ->
                            case model.moving of
                                MoveItem candidate' ->
                                    { model
                                    | moving = NoItem
                                    , preferences = insert candidate' model.preferences
                                    }
                                MovingItem candidate' _ ->
                                    { model
                                    | moving = NoItem
                                    , preferences = insert candidate' model.preferences
                                    }
                                _ ->
                                    model
                        MovedToItem candidate ->
                            case model.moving of
                                MoveItem candidate' ->
                                    { model
                                    | moving = NoItem
                                    , preferences = insertBefore candidate candidate' model.preferences
                                    }
                                MovingItem candidate' _ ->
                                    { model
                                    | moving = NoItem
                                    , preferences = insertBefore candidate candidate' model.preferences
                                    }
                                _ ->
                                    model
                        movement ->
                            { model
                            | moving = movement
                            }
    in
        (model', Cmd.none)

toggle : a -> List a -> List a
toggle item items =
    if List.member item items then
        List.filter ((/=) item) items
    else
        items ++ [item]

insert : a -> List a -> List a
insert item items =
    let
        items' = List.filter ((/=) item) items
    in
        items' ++ [item]

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
                                division
                                (ticketCandidates division ballotCandidates)
                            ViewBallot ->
                                ballotView
                                division
                                candidates
                                model.preferences
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
        Html.div [class "ballot-selection"]
            [ Html.text "Select your state"
            , Html.text " "
            , divisionSelect
            , case model.division of
                Just _ -> viewToggle
                Nothing -> Html.text ""
            ]

candidatesView : Model -> String -> List Ticket -> Html Msg
candidatesView model division tickets =
    let
        name candidate =
            [ Html.text candidate.givenName
            , Html.text " "
            , Html.text candidate.surname
            ]

        party candidate =
            [ Html.text candidate.party
            ]

        toggle candidate =
            Html.button
                [onClick <| TogglePreference candidate]
                [ if List.member candidate model.preferences then
                    Html.text "-"
                  else
                     Html.text "+"
                ]

        view candidate =
            [ toggle candidate
            , Html.span [class "name"] <| name candidate
            , Html.text " "
            , Html.span [class "party"] <| party candidate
            ]

        missingPreferences = max 0 <| 12 - List.length model.preferences

        preferences =
            Html.ol
                [ class "candidates"
                , style <| unselectable []
                ]
                (List.map preference model.preferences)

        preference candidate =
            Html.li
                [ class "candidate"
                , onMouseDown (Moving <| MoveItem candidate)
                , onMouseUp (Moving <| MovedToItem candidate)
                , style <| case model.moving of
                    NoItem -> grabCursor []
                    moving -> grabbingCursor []
                ]
                <| view candidate

        preferencesBox =
            if List.length model.preferences == 0 then
                Html.div
                    [class "preferences empty"]
                    [ Html.p
                        [ class "message"
                        , onMouseUp (Moving <| MovedToEnd)
                        ]
                        [Html.text "Drag preferences here"]
                    ]
            else if missingPreferences > 0 then
                Html.div
                    [class "preferences missing"]
                    [ preferences
                    , Html.p
                        [ class "message"
                        , onMouseUp (Moving <| MovedToEnd)
                        ]
                        [Html.text <| "Please add at least " ++ toString missingPreferences ++ " more preferences"]
                    ]
            else
                Html.div
                    [class "preferences"]
                    [ preferences
                    , Html.p
                        [ class "message"
                        , onMouseUp (Moving <| MovedToEnd)
                        ]
                        [Html.text <| "You have the required 12 preferences but you may add more"]
                    ]

        choices =
            Html.table
                [ class "choices"
                , style <| unselectable []
                ]
                [ Html.tr [] <| List.map ticketParty tickets
                , Html.tr [class "tickets"] <| List.map ticketCandidates tickets
                ]

        ticketParty ticket =
            Html.th
                [class "ticket-party"]
                [Html.text ticket.party]

        ticketCandidates ticket =
            Html.td
                []
                [ Html.ul
                    [class "candidates"]
                    (List.map choice ticket.candidates)
                ]

        choice candidate =
            Html.li
                [ class "candidate"
                , onMouseDown (Moving <| MoveItem candidate)
                , onMouseUp (Moving <| MovedToItem candidate)
                , style <| case model.moving of
                    NoItem -> grabCursor []
                    moving -> grabbingCursor []
                ]
                <| view candidate

        choicesBox =
            Html.div
                [class "choices"]
                [choices]

        moving =
            case model.moving of
                MovingItem candidate (x,y) ->
                    Html.div
                        [ class "candidate moving"
                        , style
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
            [ preferencesBox
            , choicesBox
            , moving
            ]

ballotView : String -> List Candidate -> List Candidate -> Html Msg
ballotView division candidates ballotCandidates =
    SenateBallot.ballotView
        division
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
