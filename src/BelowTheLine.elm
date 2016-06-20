module BelowTheLine exposing
    (..)

import List.Extra as List

import String
import Char
import Json.Decode as Json exposing ((:=))

import Task
import Http
import Html exposing (Html)
import Html.Attributes exposing (style, class)
import Html.Events exposing (on, onClick)
import Html.App

import BelowTheLine.Data exposing (..)
import BelowTheLine.SenateBallot as SenateBallot

-- App

type Msg
    = LoadCandidates (List Candidate)
    | LoadFailed Http.Error
    | SelectDivision String
    | ChangeView BallotView
    | AddAll (List Candidate)
    | TogglePreference Candidate
    | IncreasePreference Candidate
    | DecreasePreference Candidate

type alias Model =
    { candidates : Maybe (List Candidate)
    , ballotCandidates : Maybe (List Candidate)
    , preferences : List Candidate
    , division : Maybe String
    , ballotView : BallotView
    , error : Maybe String
    }

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
        <| fetchData "candidates.json"

-- Model

initModel : Model
initModel =
    { candidates = Nothing
    , ballotCandidates = Nothing
    , preferences = []
    , division = Nothing
    , ballotView = OrderBallot
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
                AddAll candidates ->
                    { model
                    | preferences = union candidates model.preferences
                    }
                TogglePreference candidate ->
                    { model
                    | preferences = toggle candidate model.preferences
                    }
                IncreasePreference candidate ->
                    { model
                    | preferences = moveFront candidate model.preferences
                    }
                DecreasePreference candidate ->
                    { model
                    | preferences = moveBack candidate model.preferences
                    }
    in
        (model', Cmd.none)

union : List a -> List a -> List a
union items' items =
    let
        diff = List.filter (\item -> not <| List.member item items) items'
    in
        items ++ diff

toggle : a -> List a -> List a
toggle item items =
    if List.member item items then
        List.filter ((/=) item) items
    else
        items ++ [item]

moveFront : a -> List a -> List a
moveFront item items =
    case List.elemIndex item items of
        Just place ->
            let
                front = List.take (place-1) items
                back = List.drop (place+1) items
                swap = List.take place >> List.drop (place-1) <| items
            in
                front ++ [item] ++ swap ++ back
        Nothing ->
            items

moveBack : a -> List a -> List a
moveBack item items =
    case List.elemIndex item items of
        Just place ->
            let
                front = List.take place items
                back = List.drop (place+2) items
                swap = List.take (place+2) >> List.drop (place+1) <| items
            in
                front ++ swap ++ [item] ++ back
        Nothing ->
            items


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- View

view : Model -> Html Msg
view model =
    case model.candidates of
        Just candidates ->
            Html.div []
                <| [ ballotSelection model candidates ] ++
                case (model.division, model.ballotCandidates) of
                    (Just division, Just ballotCandidates) ->
                        case model.ballotView of
                            OrderBallot ->
                                candidatesView
                                model
                                division
                                (ticketCandidates division ballotCandidates)
                            ViewBallot ->
                                [ ballotView
                                division
                                candidates
                                model.preferences ]
                    _ ->
                        [Html.text ""]
        Nothing ->
            Html.div []
            [case model.error of
                Just error -> Html.text error
                Nothing -> Html.text "Loading candidates"
            ]

ballotSelection : Model -> List Candidate -> Html Msg
ballotSelection model candidates =
    let
        noSelection = model.division == Nothing
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
        Html.div
            [class <| "ballot-selection" ++ if noSelection then " only" else ""]
            [ Html.text "Select your state"
            , Html.text " "
            , divisionSelect
            , case model.division of
                Just _ -> viewToggle
                Nothing -> Html.text ""
            ]

candidatesView : Model -> String -> List Ticket -> List (Html Msg)
candidatesView model division tickets =
    let
        allPreferenced = List.all (\candidate -> List.member candidate model.preferences)

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

        increase candidate =
            Html.button
                [class "increase", onClick <| IncreasePreference candidate]
                [Html.text "^"]

        decrease candidate =
            Html.button
                [class "decrease", onClick <| DecreasePreference candidate]
                [Html.text "v"]

        view candidate =
            [ increase candidate
            , decrease candidate
            , toggle candidate
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
                [class "candidate"]
                <| view candidate

        preferencesBox =
            if List.length model.preferences == 0 then
                Html.div
                    [class "preferences empty"]
                    [ Html.p
                        [ class "message"
                        ]
                        [Html.text "You must add 12 preferences"]
                    ]
            else if missingPreferences > 0 then
                Html.div
                    [class "preferences missing"]
                    [ preferences
                    , Html.p
                        [ class "message"
                        ]
                        [Html.text <| "Please add at least " ++ toString missingPreferences ++ " more preferences"]
                    ]
            else
                Html.div
                    [class "preferences"]
                    [ preferences
                    , Html.p
                        [ class "message"
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
                [ Html.button
                    [ onClick <| AddAll ticket.candidates
                    , Html.Attributes.disabled <| allPreferenced ticket.candidates
                    ]
                    [Html.text "+"]
                , Html.span []
                    [Html.text ticket.party]
                ]

        ticketCandidates ticket =
            Html.td
                []
                [ Html.ul
                    [class "candidates"]
                    (List.map choice ticket.candidates)
                ]

        choice candidate =
            Html.li
                [class "candidate"]
                <| view candidate

        choicesBox =
            Html.div
                [class "choices"]
                [choices]

    in
        [ preferencesBox
        , choicesBox
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

-- Events

onChange : (String -> msg) -> Html.Attribute msg
onChange msg =
    on "change" (Json.map msg (Json.at ["target", "value"] Json.string))
