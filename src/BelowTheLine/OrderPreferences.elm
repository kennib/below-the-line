module BelowTheLine.OrderPreferences exposing
    ( Msg(..)
    , view
    )

import List.Extra as List

import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)

import BelowTheLine.Data exposing (..)

-- Types
type Msg
    = AddAll (List Candidate)
    | RemoveAll (List Candidate)
    | TogglePreference Candidate
    | IncreasePreference Candidate
    | DecreasePreference Candidate

-- View

view : List Ticket -> List Candidate -> Html Msg
view tickets preferences =
    Html.div
    [class "order-preferences"]
    [ preferencesView preferences
    , choicesView tickets preferences
    ]

preferencesView : List Candidate -> Html Msg
preferencesView preferences =
    let
        missingPreferences = max 0 <| 12 - List.length preferences

        preferenceList =
            Html.ol
                [ class "candidates"
                , style <| unselectable []
                ]
                (List.map preference preferences)

        preference candidate =
            Html.li
                []
                [ Html.div
                    [class "candidate"]
                    <| candidateView candidate preferences
                ]
    in
        if List.length preferences == 0 then
            Html.div
                [class "preferences empty"]
                [ Html.p
                    [ class "message" ]
                    [Html.text "You must add 12 preferences"]
                ]
        else if missingPreferences > 0 then
            Html.div
                [class "preferences missing"]
                [ preferenceList
                , Html.p
                    [ class "message" ]
                    [Html.text <| "Please add at least " ++ toString missingPreferences ++ " more preferences"]
                ]
        else
            Html.div
                [class "preferences"]
                [ preferenceList
                , Html.p
                    [ class "message" ]
                    [Html.text <| "You have the required 12 preferences but you may add more"]
                ]

choicesView : List Ticket -> List Candidate -> Html Msg
choicesView tickets preferences =
    let
        choices =
            Html.div
                [ style <| unselectable []
                ]
                <| List.map ticketView tickets

        ticketView ticket =
            Html.div
                [class "ticket"]
                [ ticketParty ticket
                , ticketCandidates ticket
                ]

        ticketParty ticket =
            Html.div
                [class "ticket-party"]
                [ Html.button
                    [ onClick <|
                        if allPreferenced preferences ticket.candidates then
                            RemoveAll ticket.candidates
                        else
                            AddAll ticket.candidates
                    ]
                    [ if allPreferenced preferences ticket.candidates then
                        icon "remove"
                      else
                        icon "add"
                    ]
                , Html.span []
                    [Html.text ticket.party]
                ]

        ticketCandidates ticket =
            Html.ul
                [class "candidates"]
                (List.map choice ticket.candidates)

        choice candidate =
            Html.li
                [class "candidate"]
                <| candidateView candidate preferences
    in
        Html.div
            [class "choices"]
            [choices]

-- Candidate views

name candidate =
    [ Html.text candidate.givenName
    , Html.text " "
    , Html.text candidate.surname
    ]

party candidate =
    [ Html.text candidate.party
    ]

candidateView candidate preferences =
    [ Html.div
        [ class "preference-controls" ]
        [ increase candidate
        , decrease candidate
        , toggle candidate preferences
        ]
    , Html.span [class "name"] <| name candidate
    , Html.text " "
    , Html.span [class "party"] <| party candidate
    ]

toggle candidate preferences =
    Html.button
        [onClick <| TogglePreference candidate]
        [ if List.member candidate preferences then
            icon "remove"
          else
            icon "add"
         ]

increase candidate =
    Html.button
        [class "increase", onClick <| IncreasePreference candidate]
        [icon "arrow_upward"]

decrease candidate =
    Html.button
        [class "decrease", onClick <| DecreasePreference candidate]
        [icon "arrow_downward"]


-- Extra view helpers

unselectable : List (String, String) -> List (String, String)
unselectable style =
    style ++
    [ ("user-select", "none")
    , ("-webkit-user-select", "none")
    , ("user-drag", "none")
    , ("-webkit-user-drag", "none")
    ]

icon : String -> Html a
icon name =
    Html.i
        [ class "material-icons" ]
        [ Html.text name ]
