module Api exposing
    ( .. )


import Types exposing (..)
import RemoteData exposing (WebData)
import RemoteData.Http exposing (..)
import Json.Decode as D
import Json.Encode as E
import Http exposing (header)
import String exposing (fromInt)

prefix : String
prefix =
    "http://localhost:8080"


apiGetAllMessages : String
apiGetAllMessages =
    prefix ++ "/messages/all"

apiGetMessageById : String
apiGetMessageById =
    prefix ++ "/messages/getbyid/"

apiGetApprovedMessages : String
apiGetApprovedMessages =
    prefix ++ "/messages/sortedby/approve?isapproved="

apiGetMessagesByName : String
apiGetMessagesByName =
    prefix ++ "/messages/sortedby/author?authorname="

apiPostMessage : String
apiPostMessage =
    prefix ++ "/messages/create"

specialConfig : Config
specialConfig =
    { defaultConfig | headers = [ noCache, acceptJson , 
                                  header "Access-Control-Allow-Origin" "*",
                                  header "Access-Control-Allow-Methods" "GET, POST",
                                  header "Access-Control-Allow-Credentials" "true"]}

getAllMessages : (WebData (List DictEntry) -> msg) -> Cmd msg
getAllMessages msg =
    (getWithConfig defaultConfig) apiGetAllMessages msg <| D.list decodeDict

getMessageById : Int -> (WebData (DictEntry) -> msg) -> Cmd msg
getMessageById i msg =
    (getWithConfig defaultConfig) (apiGetMessageById ++ fromInt i) msg <| decodeDict

getApprovedMessages : Bool -> (WebData (List DictEntry) -> msg) -> Cmd msg
getApprovedMessages a msg = if a
    then (getWithConfig defaultConfig) (apiGetApprovedMessages ++ stringFromBool a) msg <| D.list decodeDict
    else getAllMessages msg

postMessage : (String, String) -> (WebData Int -> msg) -> Cmd msg
postMessage (c, a) message =
    let
        postBody = E.object
            [ ( "id", E.int 0 )
            , ( "content", E.string c )
            , ( "author", E.string a )
            , ( "approved", E.bool False )
            ]
    in
    post apiPostMessage message (D.field "identM" D.int) postBody


stringFromBool : Bool -> String
stringFromBool value =
  if value then
    "true"
  else
    "false"