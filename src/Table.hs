module Table(toCppHeader) where

import           Common (addHeaderDeclaration, addInclude, elementName,
                         indentation)
import           Types  (Element (..), Model (..))


toCppHeader :: Element -> String -> Int -> Model -> ( String, Model )
toCppHeader element elementParentName indentationAmount model =
    let
        eName =
            elementName element

        prefix =
            indentation indentationAmount

        instantiation =
            prefix
                ++ eName
                ++ " = new wxListCtrl("
                ++ elementParentName
                ++ ", wxID_ANY, wxDefaultPosition, wxDefaultSize, wxLC_REPORT"
                ++ ");\n\n"
                ++ prefix
                ++ "int " ++ eName ++ "_x = 0;\n"
                ++ prefix
                ++ "int " ++ eName ++ "_y = 0;\n"
                ++ prefix
                ++ "int " ++ eName ++ "_cols = 0;\n"
                ++ prefix
                ++ "int " ++ eName ++ "_rows = 0;\n"

        _code =
            instantiation
                ++ "\n"

        newModel =
            addHeaderDeclaration
                ("wxListCtrl* " ++ eName ++ ";")
                (addInclude
                    "<wx/listctrl.h>"
                    model
                )
    in
    ( _code, newModel )

-- toCppFooter :: Element -> Int -> String
-- toCppFooter element indentationAmount =
--     (indentation indentationAmount) ++ "return true;\n}"
