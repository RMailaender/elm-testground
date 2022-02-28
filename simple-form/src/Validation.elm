module Validation exposing (Valid, fromValid)

type Valid a = Valid a

fromValid : Valid a -> a
fromValid (Valid a) = a
