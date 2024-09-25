module Types exposing (..)

import Json.Decode as D


type alias DictEntry =
    { id : Int
    , content : String
    , approved : Bool
    , author : String
    }

decodeDict : D.Decoder DictEntry
decodeDict =
    D.map4 DictEntry
        (D.field "id" D.int)
        (D.field "content" D.string)
        (D.field "approved" D.bool)
        (D.field "author" D.string)