module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Person as Person



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { person : Maybe Person.Person
    , personForm : Maybe Person.FormModel
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { person = Maybe.Nothing
      , personForm = Maybe.Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ChangedPersonForm Person.FormMsg
    | ClickedCreatePerson


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedPersonForm formMsg ->
            case model.personForm of
                Just form ->
                    let
                        updated =
                            Person.formUpdate form formMsg
                    in
                    case updated.status of
                        Person.Editing ->
                            ( { model | personForm = Just updated }
                            , Cmd.none
                            )

                        Person.Submitting person ->
                            ( { model
                                | person = Just person
                                , personForm = Nothing
                              }
                            , Cmd.none
                            )

                Nothing ->
                    ( model, Cmd.none )

        ClickedCreatePerson ->
            ( { model | personForm = Just Person.newPersonForm }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        _ = Debug.log "model" model
    in
    Html.div [] <|
        ([]
            |> (\elem -> viewPersonForm model.personForm :: elem)
            |> (\elem ->
                    case model.person of
                        Just person ->
                            viewPerson person :: elem

                        Nothing ->
                            elem
               )
        )


viewPerson : Person.Person -> Html Msg
viewPerson person =
    Html.div []
        [ Html.text ("Name: " ++ (Person.name person))
        , Html.text ("Age: " ++ (Person.ageAsString person))
        ]


viewPersonForm : Maybe Person.FormModel -> Html.Html Msg
viewPersonForm formModel =
    case formModel of
        Just form ->
            Html.map ChangedPersonForm (Person.viewForm form)

        Nothing ->
            Html.button [ onClick ClickedCreatePerson ] [ Html.text "Create Person" ]
