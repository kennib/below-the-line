module BelowTheLine.Data exposing
    ( Candidate
    , House(..)
    , fetchData
    , ballotCandidates
    )

import Task exposing (Task)
import Http
import Json.Decode as Json exposing ((:=))

-- Types

type alias Candidate =
    { givenName : String
    , surname : String
    , party : String
    , house : House
    , division : String
    , ballotPosition : Int
    }

type House = Upper | Lower

-- Decoding

candidates : Json.Decoder (List Candidate)
candidates =
    Json.list candidate

candidate : Json.Decoder Candidate
candidate =
    Json.object6
        (\givenName surname party house division ballotPosition ->
            { givenName = givenName
            , surname = surname
            , party = party
            , house = house
            , division = division
            , ballotPosition = ballotPosition
            })
        ("ballot_given_nm" := Json.string)
        ("surname" := Json.string)
        ("party_ballot_nm" := Json.string)
        ("nom_ty" := Json.string `Json.andThen` house)
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

ballotCandidates : House -> String -> List Candidate -> List Candidate
ballotCandidates house division candidates =
    List.filter
        (\candidate -> candidate.house == house && candidate.division == division)
        candidates
