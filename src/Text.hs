module Text(toCppHeader) where

import           Common (addHeaderDeclaration, elementName, indentation)
import           Types  (Element (..), Model (..))


toCppHeader :: Element -> String -> Int -> String -> Model -> ( String, Model )
toCppHeader element elementParentName indentationAmount _content model =
    let
        eName =
            elementName element

        prefix =
            indentation indentationAmount

        instantiation =
            prefix
                ++ eName
                ++ " = new wxStaticText("
                ++ elementParentName
                ++ ", wxID_ANY"
                ++ ", wxT(\""
                ++ _content
                ++ "\")"
                ++ ");"
                ++ "\n"

        _code =
            instantiation
                ++ "\n"

        newModel =
            addHeaderDeclaration
                ("wxStaticText* " ++ eName ++ ";")
                model
    in
    ( _code, newModel )
