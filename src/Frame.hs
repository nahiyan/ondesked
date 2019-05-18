module Frame(toCppHeader) where

import           Common (addHeaderDeclaration, attributeValue, elementName,
                         hasClass, indentation)
import           Types  (Element (..), Model (..))


toCppHeader :: Element -> String -> Int -> Model -> ( String, Model )
toCppHeader element elementParentName indentationAmount model =
    let
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
                ++ eName
                ++ " = new wxFrame("
                ++ elementParentName
                ++ ", wxID_ANY, "
                ++ "wxT(\""
                ++ title
                ++ "\"), wxDefaultPosition, wxDefaultSize);\n\n"

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

        _code =
            instantiation
                ++ center
                ++ _show
                ++ "\n"

        newModel =
            addHeaderDeclaration
                ("wxFrame* " ++ eName ++ ";")
                model
    in
    ( _code, newModel )

-- toCppFooter :: Element -> Int -> String
-- toCppFooter element indentationAmount =
--     (indentation indentationAmount) ++ "return true;\n}"
