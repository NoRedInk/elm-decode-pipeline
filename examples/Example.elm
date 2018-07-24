module Example exposing (..)

import Json.Decode exposing (Decoder, float, int, string)
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)


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
