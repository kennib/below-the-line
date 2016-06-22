module BelowTheLine.BallotSelection exposing
    ( BallotView(..)
    , Msg(..)
    , view
    , divisionSelect
    , ballotToggle
    )

import List.Extra as List

import Json.Decode as Json exposing ((:=))

import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events exposing (on, onClick)

import BelowTheLine.Data exposing (..)

-- Types

type BallotView = OrderBallot | ViewBallot

type Msg
    = SelectDivision String
    | ChangeView BallotView

-- View

view : Maybe String -> BallotView -> List Candidate -> Html Msg
view division ballotView candidates =
    Html.div
        [class <| "ballot-selection"]
        [ divisionSelect division candidates
        , ballotToggle ballotView
        ]

divisionSelect : Maybe String -> List Candidate -> Html Msg
divisionSelect division candidates =
    let
        divisionOption default division =
            Html.option
                [Html.Attributes.selected default]
                [Html.text division]
        defaultOption =
            Html.option
                [Html.Attributes.disabled True, Html.Attributes.selected True]
                [Html.text "Select a state"]

        divisionOptions =
            defaultOption ::
            List.map
                (\division' -> divisionOption (Just division' == division) division')
                (divisions candidates)
    in
        Html.select
            [onChange SelectDivision]
            divisionOptions

ballotToggle : BallotView -> Html Msg
ballotToggle ballotView =
    case ballotView of
        OrderBallot ->
            Html.button
            [onClick <| ChangeView ViewBallot]
            [Html.text "View your ballot paper"]
        ViewBallot ->
            Html.button
            [onClick <| ChangeView OrderBallot]
            [Html.text "Change your preference order"]

-- Events

onChange : (String -> msg) -> Html.Attribute msg
onChange msg =
    on "change" (Json.map msg (Json.at ["target", "value"] Json.string))
