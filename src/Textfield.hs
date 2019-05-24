module Textfield(toCppHeader) where

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
                            "wxDefaultSize, "
                Nothing ->
                    "wxDefaultSize, "

        style =
            case attributeValue "type" element of
                Just _attributeValue ->
                    case _attributeValue of
                        "password" ->
                            "wxTE_PASSWORD"

                        "readonly" ->
                            "wxTE_READONLY"

                        _ ->
                            "wxTE_PROCESS_ENTER"
                Nothing ->
                    "wxTE_PROCESS_ENTER"

        instantiation =
            prefix
                ++ eName
                ++ " = new wxTextCtrl("
                ++ elementParentName
                ++ ", wxID_ANY, wxT(\""
                ++ elementContent
                ++ "\"), wxDefaultPosition, "
                ++ size
                ++ style
                ++ ");"
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
