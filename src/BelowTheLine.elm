module BelowTheLine exposing
    (..)

import String
import Char
import Html

main =
    Html.ol []
        <| List.map
            (\candidate -> Html.li [] <| [Html.text candidate])
            candidates

candidates : List String
candidates =
    List.map (\i -> "Candidate " ++ String.fromChar (Char.fromCode i)) [65..90]
