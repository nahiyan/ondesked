module Panel(toCppHeader) where

import           Common (addHeaderDeclaration, elementName, indentation)
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
                ++ " = new wxPanel("
                ++ elementParentName
                ++ ");"
                ++ "\n"

        _code =
            instantiation
                ++ "\n"

        newModel =
            addHeaderDeclaration
                ("wxPanel* " ++ eName ++ ";")
                model
    in
    ( _code, newModel )

-- toCppFooter :: Element -> Int -> String
-- toCppFooter element indentationAmount =
--     (indentation indentationAmount) ++ "return true;\n}"
