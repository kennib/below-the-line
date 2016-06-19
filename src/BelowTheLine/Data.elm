module BelowTheLine.Data exposing
    ( Candidate
    , House(..)
    , Ballot(..)
    , fetchData
    , ballotCandidates
    , divisions
    )

import List.Extra as List
import Task exposing (Task)
import Http
import Json.Decode as Json exposing ((:=))

-- Types

type alias Candidate =
    { givenName : String
    , surname : String
    , party : String
    , house : House
    , ballot : Ballot
    }

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

-- Functions

fetchData : String -> Task Http.Error (List Candidate)
fetchData url =
    Http.get candidates url

ballotCandidates : String -> List Candidate -> List Candidate
ballotCandidates division candidates =
    List.filter
        (\candidate -> candidateDivision candidate == division)
        candidates

divisions : List Candidate -> List String
divisions candidates =
    let
        divisions = List.map candidateDivision candidates
    in
        List.dropDuplicates <| List.sort <| divisions

candidateDivision : Candidate -> String
candidateDivision candidate =
    case candidate.ballot of
        UpperBallot ballot -> ballot.state
        LowerBallot ballot -> ballot.division
