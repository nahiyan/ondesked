module Textarea(toCppHeader) where

import           Common (attributeValue, elementName, indentation)
import           Types  (Element (..))


toCppHeader :: Element -> Maybe Element -> Int -> String -> String
toCppHeader element elementParent indentationAmount elementContent =
    let
        elementParentName =
            case elementParent of
                Just justElementParent ->
                    elementName justElementParent
                Nothing ->
                    "NULL"

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
                ++ "wxTextCtrl* "
                ++ eName
                ++ " = new wxTextCtrl("
                ++ elementParentName
                ++ ", wxID_ANY, wxT(\""
                ++ elementContent
                ++ "\"), wxDefaultPosition, "
                ++ size
                ++ "wxTE_MULTILINE);"
                ++ "\n"
    in
    instantiation
        ++ "\n"
