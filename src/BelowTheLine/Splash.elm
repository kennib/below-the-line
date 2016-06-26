module BelowTheLine.Splash exposing
    ( view
    )

import Html exposing (..)
import Html.Attributes exposing (class)

import BelowTheLine.Data exposing (..)
import BelowTheLine.BallotSelection as BallotSelection exposing (Msg(..))

view : Maybe String -> Maybe (List Candidate) -> Maybe String -> Html Msg
view division candidates error =
    div
        [ class "splash" ]
        [ h1
            []
            [ Html.text "Senate Voting" ]
        , p
            []
            [ Html.text "Prepare for your 2016 Australian Federal Election senate vote" ]
        , case candidates of
            Just candidates ->
                BallotSelection.divisionSelect division candidates
            Nothing ->
                case error of
                    Just error ->
                        p
                            [ class "error" ]
                            [ text "Oops, something has gone wrong. Try reloading the page" ]
                    Nothing ->
                        p
                            [ class "error" ]
                            [ text "Loading candidates" ]
        ]
