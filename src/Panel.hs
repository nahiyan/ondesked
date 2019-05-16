module Panel(toCppHeader) where

import           Common (elementName, indentation)
import           Types  (Element (..))


toCppHeader :: Element -> Maybe Element -> Int -> String
toCppHeader element elementParent indentationAmount =
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
                ++ "wxPanel* "
                ++ eName
                ++ " = new wxPanel("
                ++ elementParentName
                ++ ");"
                ++ "\n"
    in
    instantiation
        ++ "\n"

-- toCppFooter :: Element -> Int -> String
-- toCppFooter element indentationAmount =
--     (indentation indentationAmount) ++ "return true;\n}"
