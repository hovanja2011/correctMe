module Api exposing
    ( .. )


import Types exposing (..)
import RemoteData exposing (WebData)
import RemoteData.Http exposing (..)
import Json.Decode as D
import Http exposing (header)


prefix : String
prefix =
    "http://localhost:8080"


apiGetAllMessages : String
apiGetAllMessages =
    prefix ++ "/messages/all/"

apiGetApprovedMessages : String
apiGetApprovedMessages =
    prefix ++ "/messages/sortedby/approve?isapproved="

apiGetMessagesByName : String
apiGetMessagesByName =
    prefix ++ "/messages/sortedby/author?authorname="


specialConfig : Config
specialConfig =
    { defaultConfig | headers = [ noCache, acceptJson , 
                                  header "Access-Control-Allow-Origin" "*",
                                  header "Access-Control-Allow-Methods" "GET, POST",
                                  header "Access-Control-Allow-Credentials" "true"]}

getAllMessages : (WebData (List DictEntry) -> msg) -> Cmd msg
getAllMessages msg =
    (getWithConfig defaultConfig) apiGetAllMessages msg <| D.list decodeDict