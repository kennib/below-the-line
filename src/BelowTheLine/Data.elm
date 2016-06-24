module BelowTheLine.Data exposing
    ( Party
    , Candidate
    , CandidateId
    , House(..)
    , Ballot(..)
    , Ticket
    , fetchData
    , fetchPartiesData
    , ballotCandidates
    , divisions
    , ticketCandidates
    , senatorCount
    , candidateId
    , allPreferenced
    , partyUrl
    )

import List.Extra as List
import String

import Task exposing (Task)
import Http
import Json.Decode as Json exposing ((:=))

-- Types

type alias Party =
    { name : String
    , website : String
    }

type alias Candidate =
    { givenName : String
    , surname : String
    , party : String
    , house : House
    , ballot : Ballot
    }

type alias CandidateId = String

type House = Upper | Lower

type Ballot
    = UpperBallot
        { state : String
        , ticket : String
        , ballotPosition : Int
        }
    | LowerBallot
        { division : String
        , ballotPosition : Int
        }

type alias Ticket =
    { ticket : String
    , party : String
    , candidates : List Candidate
    }

-- Decoding

candidates : Json.Decoder (List Candidate)
candidates =
    Json.list candidate

candidate : Json.Decoder Candidate
candidate =
    Json.object5
        (\givenName surname party house ballot ->
            { givenName = givenName
            , surname = surname
            , party = party
            , house = house
            , ballot = ballot
            })
        ("ballot_given_nm" := Json.string)
        ("surname" := Json.string)
        ("party_ballot_nm" := Json.string)
        ("nom_ty" := Json.string `Json.andThen` house)
        ("nom_ty" := Json.string `Json.andThen` house `Json.andThen` ballot)

ballot : House -> Json.Decoder Ballot
ballot house =
    case house of
        Upper -> upperBallot
        Lower -> lowerBallot

upperBallot : Json.Decoder Ballot
upperBallot =
    Json.object3
        (\state ticket ballotPosition ->
            UpperBallot
            { state = state
            , ticket = ticket
            , ballotPosition = ballotPosition
            })
        ("state_ab" := Json.string)
        ("ticket" := Json.string)
        ("ballot_position" := Json.int)

lowerBallot : Json.Decoder Ballot
lowerBallot =
    Json.object2
        (\division ballotPosition ->
            LowerBallot
            { division = division
            , ballotPosition = ballotPosition
            })
        ("div_nm" := Json.string)
        ("ballot_position" := Json.int)

house : String -> Json.Decoder House
house houseCode =
    case houseCode of
        "H" -> Json.succeed Lower
        "S" -> Json.succeed Upper
        _ -> Json.fail (houseCode ++ " is not a proper house code")

parties : Json.Decoder (List Party)
parties =
    Json.list party

party : Json.Decoder Party
party =
    Json.object2
        (\name website ->
            { name = name
            , website = website
            })
        ("party" := Json.string)
        ("website" := Json.string)

-- Functions

fetchData : String -> Task Http.Error (List Candidate)
fetchData url =
    Http.get candidates url

fetchPartiesData : String -> Task Http.Error (List Party)
fetchPartiesData url =
    Http.get parties url

ballotCandidates : String -> List Candidate -> List Candidate
ballotCandidates division candidates =
    List.filter
        (\candidate -> candidateDivision candidate == division)
        candidates

divisions : List Candidate -> List String
divisions candidates =
    let
        divisions =
            List.filter (\candidate -> candidate.house == Upper) candidates
            |> List.map candidateDivision
    in
        List.dropDuplicates <| List.sort <| divisions

candidateDivision : Candidate -> String
candidateDivision candidate =
    case candidate.ballot of
        UpperBallot ballot -> ballot.state
        LowerBallot ballot -> ballot.division

ticketCandidates : String -> List Candidate -> List Ticket
ticketCandidates division candidates =
    let
        ticket candidate =
            case candidate.ballot of
                UpperBallot ballot -> ballot.ticket
                _ -> ""

        position candidate =
            case candidate.ballot of
                UpperBallot ballot -> ballot.ballotPosition
                _ -> 0

        inDivision candidate =
            case candidate.ballot of
                UpperBallot ballot -> ballot.state == division
                _ -> False

        stateCandidates =
            List.filter inDivision candidates

        ticketGroups =
            List.groupWhile (\a b -> ticket a == ticket b) stateCandidates

        ticketCandidates candidates =
            { ticket = candidatesTicket candidates
            , party = party candidates
            , candidates = candidates
            }

        candidatesTicket candidates =
            List.head candidates
            |> Maybe.map ticket
            |> Maybe.withDefault ""

        candidatesParty candidates =
            List.sortBy position candidates
            |> List.map (.party)
            |> List.dropDuplicates
            |> String.join " / "

        party candidates =
            if candidatesTicket candidates == "UG" then
                "Ungrouped"
            else
                candidatesParty candidates

        tickets =
            List.map ticketCandidates ticketGroups
    in
        tickets

senatorCount : String -> Int
senatorCount state =
    case state of
        "ACT" -> 2
        "NT"  -> 2
        _     -> 12

candidateId : Candidate -> CandidateId
candidateId candidate =
    case candidate.ballot of
        UpperBallot ballot ->
            ballot.state ++ ballot.ticket ++ toString ballot.ballotPosition
        LowerBallot ballot ->
            ballot.division ++ toString ballot.ballotPosition

allPreferenced : List Candidate -> List Candidate -> Bool
allPreferenced preferences candidates =
    List.all
        (\candidate -> List.member candidate preferences)
        candidates

partyUrl : List Party -> String -> Maybe String
partyUrl parties name =
    let
        party =
            List.find
                (\party -> party.name == name)
                parties
    in
        party
        |> Maybe.map .website
