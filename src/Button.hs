module Button(toCppHeader) where

import           Common (elementName, indentation)
import           Types  (Element (..))


toCppHeader :: Element -> Maybe Element -> Int -> String ->String
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

        instantiation =
            prefix
                ++ "wxButton* "
                ++ eName
                ++ " = new wxButton("
                ++ elementParentName
                ++ ", wxID_ANY, wxT(\""
                ++ elementContent
                ++ "\"));"
                ++ "\n"
    in
    instantiation
        ++ "\n"
