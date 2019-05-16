module Frame(toCppHeader) where

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

        title =
            case attributeValue "title" element of
                Just justTitle ->
                    justTitle
                Nothing ->
                    "Default Title"

        instantiation =
            prefix
                ++ "wxFrame* "
                ++ eName
                ++ " = new wxFrame("
                ++ elementParentName
                ++ ", wxID_ANY, "
                ++ "wxT(\""
                ++ title
                ++ "\"), wxDefaultPosition, wxDefaultSize);\n"

        center =
            if True == hasClass "centered" element then
                prefix
                    ++ eName
                    ++ "->Center();\n"
            else
                ""

        _show =
            if True == hasClass "visible" element then
                prefix
                    ++ eName
                    ++ "->Show(true);\n"
            else
                ""
    in
    instantiation
        ++ center
        ++ _show

-- toCppFooter :: Element -> Int -> String
-- toCppFooter element indentationAmount =
--     (indentation indentationAmount) ++ "return true;\n}"
