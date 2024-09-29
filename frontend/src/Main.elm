module Main exposing (main)

import Browser exposing (element)
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, placeholder, value, type_)
import Html.Events exposing (onClick, onDoubleClick, onInput, onSubmit)
import Types exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import MessageToast exposing (MessageToast)
import Api
import Maybe exposing (..)
import List


type alias TodoEdit =
    { index : Int
    , text : String
    }


type alias Model =
    { toast : MessageToast Msg
    , text : String
    , messages : List DictEntry
    
    , content : String
    , author : String

    , errorMessage : Maybe String

    , showApproved : Bool
    , spellerChecks : List (Int, String)
    }


type Msg
    = 
    Toast (MessageToast Msg)
    | TryPost
    | AddMessage (WebData Int)
    | AppendMessageIntoList (WebData DictEntry)
    | RemoveTodo Int
    | GetMessages (WebData (List DictEntry))

    | Content String
    | Author String

    | ShowApproved
    | CheckSpell (List DictEntry)

    | ShowCheckSpellResult DictEntry (WebData (List SpellerResponse)) 


init : () -> ( Model, Cmd Msg )
init flags =
    ( Model 
        (MessageToast.init Toast) 
        "" 
        [] 
        
        ""
        ""

        Nothing

        True
        []
    , Api.getAllMessages GetMessages
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toast toast ->
            ( { model | toast = toast }
            , Cmd.none
            )
        TryPost -> 
            ( model
            , Api.postMessage (model.content, model.author) AddMessage
            )
        AddMessage result ->
            case result of
                Success i ->
                    ( { model | errorMessage = Just "Сообщение добавлено" }
                    , Api.getMessageById i AppendMessageIntoList
                    )
                _ -> 
                    ( { model | errorMessage = Just "Произошла ошибка при добавлении сообщения" }
                    , Cmd.none
                    )

        AppendMessageIntoList webDe -> 
            case webDe of 
                Success de -> 
                    ( { model | messages = de :: model.messages }
                        , Cmd.none )
                _ ->  ({ model | errorMessage = Just "Получение сообщения по идентификатору не удалось"}
                        , Cmd.none)
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

        GetMessages mes ->
            case mes of
                Success messages ->
                    ( { model | messages = messages }
                    , Cmd.none
                    )
                _ ->
                    (  {model | errorMessage = Just "Произошла ошибка при получении словаря сообщений"}
                    , Cmd.none
                    )
        Content c ->
            ({ model | content = c }
            , Cmd.none)

        Author a ->
            ({ model | author = a }
            , Cmd.none)
        
        ShowApproved -> 
            ({ model | showApproved = not model.showApproved }
            , Api.getApprovedMessages model.showApproved GetMessages)
        
        CheckSpell messages -> 
            ( {model | spellerChecks = []}
            , Cmd.batch <| List.map (\m -> Api.checkMessage m (ShowCheckSpellResult m)) messages )
        
        ShowCheckSpellResult dict resW -> 
            case resW of
                Success res -> 
                    ({ model | errorMessage = Just ("Проверка удалась" )
                               , spellerChecks = (dict.id, forEveryResponseCreateList res) :: model.spellerChecks }
                    , Cmd.none)
                _ -> 
                    ( {model | errorMessage = Just "Проверка не удалась"}
                    , Cmd.none )


forEveryResponseCreateList : List SpellerResponse -> String
forEveryResponseCreateList s = List.foldl (\l r -> l.word ++ " (" ++ takeFirstGoodWord l ++ "), " ++ r) "" s

takeFirstGoodWord : SpellerResponse -> String
takeFirstGoodWord r = withDefault "" (List.head r.s)


view : Model -> Html Msg
view model =
    div [ class "col-12 col-sm-6 offset-sm-3" ]
        [ div [ class "input-block"]
            [ viewInput "text" "Content" model.content Content
            , viewInput "text" "Author" model.author Author
            , button
                [ class "btn btn-primary form-control" 
                , onClick TryPost]
                [ text "+" ]
            ]
        , div [ class "checkbox-block"]
            [ span
                [ ]
                [ text "Только подтвержденные" ]
            , input [ type_ "checkbox"
                    , onClick ShowApproved] []
            ]
        , span
                [ ]
                [ text (withDefault "Ошибки при работе: Нет" model.errorMessage) ]
        , div [] [
            button
                [ class "btn btn-primary form-control" 
                    , onClick (CheckSpell model.messages)]
                    [ text "yandex speller, check all messages" ]
                ]
        , hr [] []
        , div [ class "header" ] [
            span [] [ text "Сожержание сообщения" ]
            , span [] [ text "     |     " ]
            , span [] [ text "Автор" ]
            , span [] [ text "     |     " ]
            , span [] [ text "Статус подтверждения" ]
            , span [] [ text "     |     " ]
            , span [] [ text "Ошибочное слово (вариант правильного написания)" ]
            ]
        , span [] [ text "--------------------------------------------------------------------------------------------------------------------------------------" ]
        ,
         div [] (List.map (viewMessages model.spellerChecks) model.messages)
        ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewMessages : List (Int, String) -> DictEntry -> Html Msg
viewMessages checks message = 
    div [ class "card" ]
        [ div [ class "card-block" ]
            [ span
                [ ]
                [ text message.content ]
            , span
                [ ]
                [ text "     |     "]
            , span
                [ ]
                [ text message.author ]
            , span
                [ ]
                [ text "     |     "]
            , span
                [ ]
                [ text (case message.approved of 
                            True -> "Подтвержден"
                            False -> "Не подтвержден") ]
            , span
                [ ]
                [ text "     |     "]
            , span
                [ ]
                [ text <| elem message.id checks]
            -- , span
            --     [ onClick (RemoveTodo index)
            --     , class "float-right"
            --     ]
            --     [ text "✖" ]
            ]
        ]

elem : Int -> List (Int, String) -> String
elem index list =  case List.filter (\(i, _) -> i == index) list of
    [] -> ""
    [(i, s)] -> s
    _ -> "Слишком много записей для данного идентификатора"


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
