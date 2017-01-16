module Formo exposing (..)

import Dict exposing (Dict)


-- MODEL


type alias Element =
    { errors : List ValidationError
    , focused : Bool
    , validators : List ElementValidator
    , value : String
    }


type alias Elements =
    Dict String Element


type alias ElementValidator =
    String -> ValidationError


type alias Form =
    { elements : Elements
    }


type alias ValidationError =
    Maybe String


initForm : List ( String, List ElementValidator ) -> Form
initForm fields =
    let
        elements =
            List.foldl
                (\e -> Dict.insert (Tuple.first e) (initElement e))
                Dict.empty
                fields
    in
        Form elements


initElement : ( String, List ElementValidator ) -> Element
initElement ( _, validators ) =
    Element [] False validators ""


blurElement : Form -> String -> Form
blurElement form name =
    case Dict.get name form.elements of
        Nothing ->
            form

        Just element ->
            let
                elements =
                    Dict.insert
                        name
                        { element | focused = False }
                        form.elements
            in
                { form | elements = elements }

elementError : Form -> String -> String
elementError form name =
    ""

elementValue : Form -> String -> String
elementValue form name =
    case Dict.get name form.elements of
        Nothing ->
            ""

        Just element ->
            element.value

focusElement : Form -> String -> Form
focusElement form name =
    case Dict.get name form.elements of
        Nothing ->
            form

        Just element ->
            let
                elements =
                    Dict.insert
                        name
                        { element | focused = True }
                        form.elements
            in
                { form | elements = elements }

updateElementValue : Form -> String -> String -> Form
updateElementValue form name value =
    case Dict.get name form.elements of
        Nothing ->
            form

        Just element ->
            let
                elements =
                    Dict.insert
                        name
                        { element | value = value }
                        form.elements
            in
                { form | elements = elements }
