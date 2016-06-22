module BelowTheLine.BallotSelection exposing
    ( BallotView(..)
    , Msg(..)
    , view
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
    let
        noSelection = division == Nothing
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

        divisionSelect =
            Html.select
                [onChange SelectDivision]
                divisionOptions

        viewToggle =
            case ballotView of
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
            , case division of
                Just _ -> viewToggle
                Nothing -> Html.text ""
            ]

-- Events

onChange : (String -> msg) -> Html.Attribute msg
onChange msg =
    on "change" (Json.map msg (Json.at ["target", "value"] Json.string))
