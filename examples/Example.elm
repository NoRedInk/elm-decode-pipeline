module Example exposing (..)

import Json.Decode exposing (int, string, float, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


type alias User =
    { id : Int
    , name : String
    , percentExcited : Float
    }


userDecoder : Decoder User
userDecoder =
    decode User
        |> required "id" int
        |> optional "name" string "(fallback if name not present)"
        |> hardcoded 1.0
