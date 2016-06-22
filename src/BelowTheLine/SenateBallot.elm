module BelowTheLine.SenateBallot exposing
    ( ballotView
    )

import List.Extra as List

import Html exposing (table, tr, td, div, h1, h2, p, br, span, strong, text)
import Html.Attributes exposing (class, style, colspan)

import BelowTheLine.Data exposing (senatorCount)

ballotView division tickets order =
  table
    [ class "ballot-paper-sen"
    , style
      [ ("width", (toString <| List.length tickets * 110) ++ "px")
      ]
    ]
    [ tr [class "ballot-header"]
      [ td [class "ballot-header-title", colspan <| List.length tickets + 1]
        [ text "Senate Ballot Paper"
        , br [] []
        , strong [] [text <| fullName division]
        , text <| " - Election of " ++ toString (senatorCount division) ++ " Senators"
        ]
      ]
    , tr [class "ballot-space"] []
    , tr [class "ballot-option-a"]
      ([ td [class "option-a-header"]
        [ h2 []
          [ text "You may"
          , br [] []
          , text "vote in one of"
          , br [] []
          , text "two ways"
          ]
        , p [class "senate-option"] [text "Either"]
        , h2 [] [text "Above the line"]
        , p []
          [ text "By numbering at least "
          , strong [] [text "6"]
          , br [] []
          , text "of these boxes in the order"
          , br [] []
          , text "of your choice (with number"
          , br [] []
          , text "1 as your first choice)"
          ]
        ]
      ]
      ++ (List.map ticketView tickets))
    , tr [class "ballot-space"] []
    , tr [class "ballot-line"] [ td [colspan <| List.length tickets + 1] [Html.text "-"]]
    , tr [class "ballot-space"] []
    , tr [class "ballot-option-b"]
      ([ td [class "option-b-header"]
        [ p [class "senate-option"] [text "Or"]
        , h2 [] [text "Below the line"]
        , p []
          [ text "By numbering at least "
          , strong [] [text "12"]
          , br [] []
          , text "of these boxes in the order"
          , br [] []
          , text "of your choice (with number"
          , br [] []
          , text "1 as your first choice)."
          ]
        ]
      ]
      ++ (List.map (belowTicketView order) tickets))
    ]

ticketView ticket =
  case ticket.ticket of
    "UG" ->
      td [class "ballot-position"]
        [ div [class "ballot-number blank"] [ ]
        , div [class "ballot-party"] [ ]
        ]
    _ ->
      td [class "ballot-position"]
        [ div [class "ballot-number"]
          [ text ticket.ticket
          , div [class "ballot-logo-sen blank"] []
          , div [class "ballot-logo-sen"] []
          , div [class "ballot-box"] []
          ]
        , div [class "ballot-party"] [text ticket.party]
        ]

belowTicketView order ticket =
  let
    party =
      case ticket.ticket of
        "UG" -> "Ungrouped"
        _ -> ticket.party

    number candidate = List.elemIndex candidate order |> Maybe.map ((+) 1)

    candidates =
      List.map
        (\candidate -> candidateView candidate <| number candidate)
        ticket.candidates

  in
    td [class "below-ballot-position"]
      (p [class "party-title"] [text party]
      :: candidates)

candidateView candidate number =
  div [class "ballot-candidate-group"]
    [ div [class "ballot-number"]
      [ div [class "ballot-box"] [number |> Maybe.map toString |> Maybe.withDefault "" |> Html.text] ]
    , div [class "ballot-candidate"]
      [ text candidate.surname
      , br [] []
      , span []
        [ text candidate.givenName
        , br [] []
        , span [] [text candidate.party]
        ]
      ]
    ]

fullName division =
    case division of
        "ACT" -> "Australian Capital Territory"
        "NSW" -> "New South Wales"
        "NT" -> "Northern Territory"
        "QLD" -> "Queensland"
        "SA" -> "South Australia"
        "TAS" -> "Tasmania"
        "VIC" -> "Victoria"
        "WA" -> "Western Australia"
        _ -> division
