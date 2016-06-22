module BelowTheLine exposing
    (..)

import List.Extra as List

import String

import Task
import Http
import Navigation exposing (Location, modifyUrl, newUrl)
import Html exposing (Html)
import Html.Attributes exposing (style, class)
import Html.App

import BelowTheLine.Data exposing (..)
import BelowTheLine.BallotSelection as BallotSelection exposing (BallotView(..), Msg(..))
import BelowTheLine.OrderPreferences as OrderPreferences exposing (Msg(..))
import BelowTheLine.SenateBallot as SenateBallot

-- App

type Msg
    = LoadCandidates (List CandidateId) (List Candidate)
    | LoadFailed Http.Error
    | BallotSelection BallotSelection.Msg
    | OrderPreferences OrderPreferences.Msg

type alias Model =
    { candidates : Maybe (List Candidate)
    , ballotCandidates : Maybe (List Candidate)
    , preferences : List Candidate
    , division : Maybe String
    , ballotView : BallotView
    , error : Maybe String
    }

type alias UrlData =
    { division : Maybe String
    , ballotView : BallotView
    , preferences : List CandidateId
    }

main =
    Navigation.program
        (Navigation.makeParser urlParser)
        { init = init
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        , view = view
        }

-- URL

urlParser : Location -> UrlData
urlParser location =
    let
        params =
            location.search
            |> String.dropLeft 1
            |> String.split "&"
            |> List.map (String.split "=")

        getParam key =
            List.find
                (\entry -> getKey entry == key)
                params
        getKey entry =
            case entry of
                key::_ -> key
                [] -> ""
        getValue entry =
            case entry of
                key::value::_ -> value
                [key] -> key
                [] -> ""

        division =
            getParam "division"
            |> Maybe.map getValue
        ballotView =
            getParam "ballotView"
            |> Maybe.map getValue
            |> (flip Maybe.andThen) ballotViewFromString
            |> Maybe.withDefault OrderBallot
        ballotViewFromString string =
            case string of
                "OrderBallot" -> Just OrderBallot
                "ViewBallot" -> Just ViewBallot
                _ -> Nothing
        preferences =
            getParam "preferences"
            |> Maybe.map getValue
            |> Maybe.map (String.split ",")
            |> Maybe.withDefault []
    in
        { division = division
        , ballotView = ballotView
        , preferences = preferences
        }

urlMaker : Model -> String
urlMaker model =
    let
        division = model.division |> Maybe.withDefault ""
        ballotView = toString model.ballotView
        preferences =
            List.map candidateId model.preferences
            |> String.join ","
    in
        "?division=" ++ division
        ++ "&ballotView=" ++ ballotView
        ++ "&preferences=" ++ preferences

-- Model

init : UrlData -> (Model, Cmd Msg)
init urlData =
    let
        model =
            { candidates = Nothing
            , ballotCandidates = Nothing
            , preferences = []
            , division = urlData.division
            , ballotView = urlData.ballotView
            , error = Nothing
            }
    in
        (model, fetchPreferences urlData.preferences)

fetchPreferences : List CandidateId -> Cmd Msg
fetchPreferences preferences =
    Task.perform LoadFailed (LoadCandidates preferences)
        <| fetchData "candidates.json"

getPreferences : List CandidateId -> List Candidate -> List Candidate
getPreferences candidateIds candidates =
    List.filterMap
        (\id -> List.find (\candidate -> candidateId candidate == id) candidates)
        candidateIds

-- Update

urlUpdate : UrlData -> Model -> (Model, Cmd Msg)
urlUpdate data model =
    let
        candidates =
            Maybe.map2 ballotCandidates
                data.division
                model.candidates

        model' =
            { model
            | division = data.division
            , ballotCandidates = candidates
            , ballotView = data.ballotView
            , preferences = preferences
            }

        preferences =
            Maybe.map
                (getPreferences data.preferences)
                model.candidates
            |> Maybe.withDefault []
    in
        (model', Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        model' =
            case msg of
                LoadCandidates preferences candidates ->
                    { model
                    | candidates = Just candidates
                    , ballotCandidates = Maybe.map (flip ballotCandidates <| candidates) model.division
                    , preferences = getPreferences preferences candidates
                    }
                LoadFailed error ->
                    { model
                    | error = Just <| toString error
                    }
                BallotSelection msg ->
                    case msg of
                        SelectDivision division ->
                            { model
                            | division = Just division
                            , ballotCandidates = Maybe.map (ballotCandidates division) model.candidates
                            , preferences = []
                            }
                        ChangeView view ->
                            { model
                            | ballotView = view
                            }
                OrderPreferences msg ->
                    case msg of
                        AddAll candidates ->
                            { model
                            | preferences = union candidates model.preferences
                            }
                        RemoveAll candidates ->
                            { model
                            | preferences = difference candidates model.preferences
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

        updateUrl = modifyUrl <| urlMaker model'
        addUrl = newUrl <| urlMaker model'
        cmd =
            case msg of
                BallotSelection msg ->
                    case msg of
                        SelectDivision _ ->
                            addUrl
                        ChangeView _ ->
                            updateUrl
                OrderPreferences msg ->
                    case msg of
                        AddAll _ ->
                            updateUrl
                        RemoveAll _ ->
                            updateUrl
                        TogglePreference _ ->
                            updateUrl
                        IncreasePreference _ ->
                            updateUrl
                        DecreasePreference _ ->
                            updateUrl
                _ ->
                    Cmd.none
    in
        (model', cmd)

union : List a -> List a -> List a
union items' items =
    let
        diff = List.filter (\item -> not <| List.member item items) items'
    in
        items ++ diff

difference : List a -> List a -> List a
difference items' items =
    List.filter (\item -> not <| List.member item items') items

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
                <| [ BallotSelection.view
                    model.division
                    model.ballotView
                    candidates
                    |> Html.App.map BallotSelection
                ] ++
                case (model.division, model.ballotCandidates) of
                    (Just division, Just ballotCandidates) ->
                        case model.ballotView of
                            OrderBallot ->
                                OrderPreferences.view
                                division
                                (ticketCandidates division ballotCandidates)
                                model.preferences
                                |> List.map (Html.App.map OrderPreferences)
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

ballotView : String -> List Candidate -> List Candidate -> Html Msg
ballotView division candidates ballotCandidates =
    SenateBallot.ballotView
        division
        (ticketCandidates division candidates)
        ballotCandidates
