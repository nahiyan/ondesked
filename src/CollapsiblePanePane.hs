module CollapsiblePanePane(toCppHeader) where

import           Common (addHeaderDeclaration, attributeValue, elementName,
                         indentation)
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
                ++ " = "
                ++ elementParentName
                ++ "->GetPane();"
                ++ "\n"


        newModel =
            addHeaderDeclaration
                ("wxWindow* " ++ eName ++ ";")
                model


        _code =
            instantiation
                ++ "\n"
    in
    ( _code, newModel )
