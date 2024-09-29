module Types exposing (..)

import Json.Decode as D


type alias DictEntry =
    { id : Int
    , content : String
    , author : String
    , approved : Bool
    }

decodeDict : D.Decoder DictEntry
decodeDict =
    D.map4 DictEntry
        (D.field "id" D.int)
        (D.field "content" D.string)
        (D.field "author" D.string)
        (D.field "approved" D.bool)