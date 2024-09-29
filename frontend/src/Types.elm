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

---
type alias SpellerRequest =
    { text : String
    , lang : String
    , options : String
    , format : String
    }

decodeSpellerRequest : D.Decoder SpellerRequest
decodeSpellerRequest =
    D.map4 SpellerRequest
        (D.field "text" D.string)
        (D.field "lang" D.string)
        (D.field "options" D.string)
        (D.field "format" D.string)

---
type alias SpellerResponse =
    {         
        code : Int,
        pos : Int,
        row : Int,
        col : Int,
        len : Int,
        word : String,
        s : List String
    }

decodeSpellerResponse : D.Decoder SpellerResponse
decodeSpellerResponse =
    D.map7 SpellerResponse
        (D.field "code" D.int)
        (D.field "pos" D.int)
        (D.field "row" D.int)
        (D.field "col" D.int)
        (D.field "len" D.int)
        (D.field "word" D.string)
        (D.field "s" (D.list D.string))

defaultSpellerResponse : SpellerResponse
defaultSpellerResponse = SpellerResponse 0 0 0 0 0 "" []