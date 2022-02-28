module Person exposing (..)

import Validation exposing (..)

type PersonId = PersonId String

type alias Person =
    { id : PersonId 
    , name : String
    , age : Int 
    }

-- FORM

type alias PersonFormInput =
    { name : String 
    , age : String
    }

type alias NewPersonFormOut =
    { name : String 
    , age : Int 
    }

createPerson : (Valid NewPersonFormOut) -> Person
createPerson validPerson =
    let
        newPerson = fromValid validPerson
    in
    { id = PersonId ""
    , name = newPerson.name
    , age = newPerson.age 
    }