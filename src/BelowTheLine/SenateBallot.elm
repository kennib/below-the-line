module BelowTheLine.SenateBallot exposing
    ( ballotView
    )

import Html exposing (div, h1, h2, p, hr, br, span, strong, text)
import Html.Attributes exposing (class, style)

ballotView tickets =
  div
    [ class "responsive-container"
    , style [("width", toString(164+List.length tickets*142)++"px")]
    ]
    [ div [class "ballot-paper-sen"]
      [ div [class "ballot-header-title"]
        [ p []
          [ text "Senate Ballot Paper"
          , br [] []
          , strong [] [text "State"]
          , text " - Election of 12 Senators"
          ]
        ]
      , div [class "ballot-option-a"]
        [ div [class "option-a-header"]
          [ h2 []
            [ text "You may"
            , br [] []
            , text "vote in one of"
            , br [] []
            , text "two ways"
            ]
          , p [class "senate-option"] [text "Either:"]
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
        , div [class "option-a-select"]
          (List.map ticketView tickets)
        ]
      , hr [class "ballot-line"] []
      , div [class "ballot-option-b"]
        [ div [class "option-b-header"]
          [ p [class "senate-option"] [text "Or:"]
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
        , div [class "option-b-select"]
          ((List.map belowTicketView tickets)
          ++ [ div [class "clearer"] [] ])
        ]
      ]
    ]

ticketView ticket =
  case ticket.ticket of
    "UG" ->
      div [class "ballot-position"]
        [ div [class "ballot-number blank"] [ ]
        , div [class "ballot-party"] [ ]
        ]
    _ ->
      div [class "ballot-position"]
        [ div [class "ballot-number"]
          [ text ticket.ticket
          , div [class "ballot-logo-sen blank"] []
          , div [class "ballot-logo-sen"] []
          , div [class "ballot-box"] []
          ]
        , div [class "ballot-party"] [text ticket.party]
        ]

belowTicketView ticket =
  let
    party =
      case ticket.ticket of
        "UG" -> "Ungrouped"
        _ -> ticket.party

    candidates = List.map candidateView ticket.candidates

  in
    div [class "below-ballot-position"]
      (p [class "party-title"] [text party]
      :: candidates)

candidateView candidate =
  div [class "ballot-candidate-group"]
    [ div [class "ballot-number"]
      [ div [class "ballot-box"] [] ]
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
