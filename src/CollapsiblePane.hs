module CollapsiblePane(toCppHeader) where

import           Common (addHeaderDeclaration, addInclude, attributeValue,
                         elementName, indentation)
import           Types  (Element (..), Model (..))


toCppHeader :: Element -> String -> Int -> Model -> ( String, Model)
toCppHeader element elementParentName indentationAmount model =
    let
        eName =
            elementName element

        prefix =
            indentation indentationAmount

        _name =
            case attributeValue "name" element of
                Just justName ->
                    justName
                Nothing ->
                    ""

        instantiation =
            prefix
                ++ eName
                ++ " = new wxCollapsiblePane("
                ++ elementParentName
                ++ ", wxID_ANY, wxT(\""
                ++ _name
                ++ "\"));"
                ++ "\n"

        newModel =
            addInclude
                "<wx/collpane.h>"
                (addHeaderDeclaration
                    ("wxCollapsiblePane* " ++ eName ++ ";")
                    model
                )


        _code =
            instantiation
                ++ "\n"
    in
    ( _code, newModel )
