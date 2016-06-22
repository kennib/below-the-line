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

view : String -> List Ticket -> List Candidate -> List (Html Msg)
view division tickets preferences =
    let
        allPreferenced = List.all (\candidate -> List.member candidate preferences)

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

        view candidate =
            [ increase candidate
            , decrease candidate
            , toggle candidate
            , Html.span [class "name"] <| name candidate
            , Html.text " "
            , Html.span [class "party"] <| party candidate
            ]

        missingPreferences = max 0 <| 12 - List.length preferences

        preferenceList =
            Html.ol
                [ class "candidates"
                , style <| unselectable []
                ]
                (List.map preference preferences)

        preference candidate =
            Html.li
                [class "candidate"]
                <| view candidate

        preferencesBox =
            if List.length preferences == 0 then
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
                    [ preferenceList
                    , Html.p
                        [ class "message"
                        ]
                        [Html.text <| "Please add at least " ++ toString missingPreferences ++ " more preferences"]
                    ]
            else
                Html.div
                    [class "preferences"]
                    [ preferenceList
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
                    [ onClick <|
                        if allPreferenced ticket.candidates then
                            RemoveAll ticket.candidates
                        else
                            AddAll ticket.candidates
                    ]
                    [ if allPreferenced ticket.candidates then
                        icon "remove"
                      else
                        icon "add"
                    ]
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
        [ class "material-icons md-24" ]
        [ Html.text name ]
