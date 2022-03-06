module Age exposing
    ( Age
    , fromInt
    )


type Age
    = Age Int


fromInt : Int -> Maybe Age
fromInt age =
    if age >= 0 then
        Just age

    else
        Nothing
