module Api exposing
    ( .. )


prefix : String
prefix =
    ""


apiGetMessages : String
apiGetMessages =
    prefix ++ "/messages/all"
