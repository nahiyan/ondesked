module Panel(toCppHeader) where

import           Common (elementName, indentation)
import           Types  (Element (..))


toCppHeader :: Element -> String -> Int -> String
toCppHeader element elementParentName indentationAmount =
    let
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
