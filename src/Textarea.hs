module Textarea(toCppHeader) where

import           Common (addHeaderDeclaration, attributeValue, elementName,
                         indentation)
import           Types  (Element (..), Model (..))


toCppHeader :: Element -> String -> Int -> String -> Model -> ( String, Model)
toCppHeader element elementParentName indentationAmount elementContent model =
    let
        eName =
            elementName element

        prefix =
            indentation indentationAmount

        size =
            case attributeValue "width" element of
                Just justWidth ->
                    case attributeValue "height" element of
                        Just justHeight ->
                            "wxSize("
                                ++ justWidth
                                ++ ", "
                                ++ justHeight
                                ++ "), "

                        Nothing ->
                            ""
                Nothing ->
                    ""

        instantiation =
            prefix
                ++ eName
                ++ " = new wxTextCtrl("
                ++ elementParentName
                ++ ", wxID_ANY, wxT(\""
                ++ elementContent
                ++ "\"), wxDefaultPosition, "
                ++ size
                ++ "wxTE_MULTILINE);"
                ++ "\n"

        newModel =
            addHeaderDeclaration
                ("wxTextCtrl* " ++ eName ++ ";")
                model

        _code =
            instantiation
                ++ "\n"
    in
    ( _code, newModel )
