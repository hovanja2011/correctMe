module Utils exposing (..)


stringFromBool : Bool -> String
stringFromBool value =
  if value then
    "true"
  else
    "false"
