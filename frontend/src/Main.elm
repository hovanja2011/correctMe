module Main exposing (main)

import Browser exposing (element)
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, placeholder, value)
import Html.Events exposing (onClick, onDoubleClick, onInput, onSubmit)
import Types exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import MessageToast exposing (MessageToast)
import Api


type alias TodoEdit =
    { index : Int
    , text : String
    }


type alias Model =
    { toast : MessageToast Msg
    , text : String
    , messages : List DictEntry
    }


type Msg
    = 
    Toast (MessageToast Msg)
    | UpdateText String
    | AddMessage
    | RemoveTodo Int
    | GetAllMessages (WebData (List DictEntry))


init : () -> ( Model, Cmd Msg )
init flags =
    ( Model 
        (MessageToast.init Toast) 
        "" 
        [] 
    , Api.getAllMessages GetAllMessages
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        showError : String -> Model
        showError e =
            let
                toast : MessageToast Msg
                toast =
                    model.toast
                        |> MessageToast.danger
                        |> MessageToast.withMessage e
            in
            { model | toast = toast }
    in
    case msg of
        Toast toast ->
            ( { model | toast = toast }
            , Cmd.none
            )
        UpdateText newText ->
            ( { model | text = newText }, Cmd.none )

        AddMessage ->
            ( { model | text = "", messages = model.messages }
            , Cmd.none
            )

        RemoveTodo index ->
            let
                beforemessages =
                    List.take index model.messages

                aftermessages =
                    List.drop (index + 1) model.messages

                newmessages =
                    beforemessages ++ aftermessages
            in
            ( { model | messages = newmessages }, Cmd.none )

        GetAllMessages mes ->
            case mes of
                Success messages ->
                    ( { model | messages = messages }
                    , Cmd.none
                    )
                _ ->
                    ( showError "Произошла ошибка при получении словаря сообщений"
                    , Cmd.none
                    )


view : Model -> Html Msg
view model =
    div [ class "col-12 col-sm-6 offset-sm-3" ]
        [ form [ class "row", onSubmit AddMessage ]
            [ div [ class "col-9" ]
                [ input
                    [ onInput UpdateText
                    , value model.text
                    , autofocus True
                    , class "form-control"
                    , placeholder "Enter a todo"
                    ]
                    []
                ]
            , div [ class "col-3" ]
                [ button
                    [ class "btn btn-primary form-control" ]
                    [ text "+" ]
                ]
            ]
        , div [] (List.indexedMap viewMessages model.messages)
        ]


viewMessages : Int -> DictEntry -> Html Msg
viewMessages index message = 
    div [ class "card" ]
        [ div [ class "card-block" ]
            [ span
                [ ]
                [ text message.content ]
            , span
                [ onClick (RemoveTodo index)
                , class "float-right"
                ]
                [ text "✖" ]
            ]
        ]



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }