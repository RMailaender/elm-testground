module Set exposing (..)


type Set comparable
    = Tree comparable (Set comparable) (Set comparable)
    | Empty


empty : Set comparable
empty =
    Empty


singleton : comparable -> Set comparable
singleton item =
    Tree item empty empty


insert : comparable -> Set comparable -> Set comparable
insert item set =
    case set of
        Empty ->
            singleton item

        Tree head left right ->
            if item < head then
                Tree head (insert item left) right

            else if item > head then
                Tree head left (insert item right)

            else
                set


fromList : List comparable -> Set comparable
fromList items =
    List.foldl insert empty items
