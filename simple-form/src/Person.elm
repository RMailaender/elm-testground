module Person exposing
    ( FormModel
    , FormMsg
    , FormStatus(..)
    , Person
    , ageAsString
    , formUpdate
    , name
    , newPersonForm
    , viewForm
    )

import Html
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)



-- PERSON


type Person
    = Person Internal


type alias Internal =
    { name : String
    , age : Int
    }


name : Person -> String
name (Person person) =
    person.name


ageAsString : Person -> String
ageAsString (Person { age }) =
    String.fromInt age



-- FORM


type FormMsg
    = EnteredName String
    | EnteredAge String
    | SubmittedPerson Person


formUpdate : FormModel -> FormMsg -> FormModel
formUpdate model msg =
    case msg of
        EnteredAge age ->
            formInputUpdate model <|
                \form -> { form | age = age }

        EnteredName newName ->
            formInputUpdate model <|
                \form -> { form | name = newName }

        SubmittedPerson person ->
            { model | status = Submitting person }


formInputUpdate : FormModel -> (PersonFormInput -> PersonFormInput) -> FormModel
formInputUpdate model updateField =
    { model | form = updateField model.form }


type FormStatus
    = Editing
    | Submitting Person


type alias FormModel =
    { status : FormStatus
    , form : PersonFormInput
    }


type alias PersonFormInput =
    { name : String
    , age : String
    }


newPersonForm : FormModel
newPersonForm =
    { status = Editing
    , form = inputInit
    }


inputInit : PersonFormInput
inputInit =
    { name = ""
    , age = ""
    }


parseName : String -> Result String String
parseName nameInput =
    let
        trimmed =
            String.trim nameInput
    in
    if String.length trimmed > 0 then
        Ok trimmed

    else
        Err "Name to short."


parseAge : String -> Result String Int
parseAge ageInput =
    case String.toInt ageInput of
        Just age ->
            if age > 0 then
                Ok age

            else
                Err "Age musst be greater then 0."

        Nothing ->
            Err "That's not an int"


parseFormData : PersonFormInput -> Result String Person
parseFormData input =
    Result.map2
        (\inputName inputAge -> Person { name = inputName, age = inputAge })
        (parseName input.name)
        (parseAge input.age)



-- VIEW


inputView : String -> String -> (String -> msg) -> String -> Html.Html msg
inputView t p toMsg v =
    Html.input [ Attr.type_ t, Attr.value v, Attr.placeholder p, onInput toMsg ] []


viewForm : FormModel -> Html.Html FormMsg
viewForm { form } =
    let
        submitButton =
            case parseFormData form of
                Ok person ->
                    Html.button
                        [ onClick (SubmittedPerson person) ]
                        [ Html.text "Submit" ]

                Err _ ->
                    Html.button [ Attr.disabled True ] [ Html.text "Submit" ]
    in
    Html.div []
        [ inputView "text" "Name" EnteredName form.name
        , inputView "number" "Age" EnteredAge form.age
        , submitButton
        ]
