module BoxSizer(toCppHeader) where

import           Common (attributeValue, elementName, hasClass, indentation)
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

        orientation =
            if True == hasClass "horizontal" element then
                "wxHORIZONTAL"
            else
                "wxVERTICAL"

        instantiation =
            prefix
                ++ "wxBoxSizer* "
                ++ eName
                ++ " = new wxBoxSizer("
                ++ orientation ++ ");\n"
    in
    instantiation

-- toCppFooter :: Element -> Int -> String
-- toCppFooter element indentationAmount =
--     (indentation indentationAmount) ++ "return true;\n}"
