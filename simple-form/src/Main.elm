module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Debug

-- MAIN

main : Program () Model Msg
main = Browser.element 
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model = FormInput

init : () -> ( Model, Cmd Msg )
init _ = ( formInit, Cmd.none )

-- UPDATE
type Msg = FormChanged FormMsg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        FormChanged formMsg ->
            (formUpdate formMsg model, Cmd.none )




-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- VIEW

view : Model -> Html Msg
view model =
    Html.div []
        [ Html.map FormChanged (formView model) ]


-- FORM
type Parsed a
    = Valid a
    | Invalid (String, String)

-- -- MODEL
type alias FormInput =
    { name : String 
    , age : String 
    }

formInit : FormInput
formInit = FormInput "" ""

parseName : String -> Parsed String
parseName rawName =
    if String.isEmpty rawName then 
        Invalid ("Name musst not be empty.", rawName)
    else 
        Valid rawName

parseAge : String -> Parsed Int
parseAge rawAge =
    case (String.toInt rawAge) of
        Just age ->
            Valid age
        
        Nothing ->
            Invalid ("Invalid Age.", rawAge)

-- -- UPDATE

type FormMsg
    = NameInputChanged String
    | AgeInputChanged String

formUpdate : FormMsg -> FormInput -> FormInput
formUpdate msg input =
    case msg of
        NameInputChanged name ->
            { input | name = name }

        AgeInputChanged age ->
            { input | age = age}

-- -- VIEW

inputView : String -> String -> (String -> msg) -> String -> Html msg
inputView t p toMsg v =
    Html.input [ type_ t, value v, placeholder p, onInput toMsg ] [ ]

formView : FormInput -> Html FormMsg
formView form =
    let
        _ = Debug.log "Name: " form.name
        _ = Debug.log "Age: " form.age


    in
    Html.form [] 
        [ inputView "text" "Name" NameInputChanged form.name
        , inputView "number" "Age" AgeInputChanged form.age
        ]