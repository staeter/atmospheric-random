module Random.Atmospheric exposing (..)

import Http
import Hex

{-| Request random.org for a given amount of random numbers.
    Amount will max out at 10000 and Cmd.none will be returned if it is less than 1.
    The cmd will come back with an empty list in case of error.
-}
request : Int -> Cmd (List Int)
request amount =
    let
        amountString =
            String.fromInt (min amount 10000)

        url =
            "https://www.random.org/integers/?num="
            ++ amountString
            ++ "&min=-1000000000&max=1000000000&col="
            ++ amountString
            ++ "&base=16&format=plain&rnd=new"
    in
    if amount < 1 then
        Cmd.none

    else
        Http.get { url = url, expect = Http.expectString identity }
            |> Cmd.map
                ( Result.map
                    ( String.trim
                        >> String.split "\t"
                        >> List.filterMap (Hex.fromString >> Result.toMaybe)
                    )
                    >> Result.toMaybe
                    >> Maybe.withDefault []
                )
