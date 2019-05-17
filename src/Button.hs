module Button(toCppHeader) where

import           Common (elementName, indentation)
import           Types  (Element (..))


toCppHeader :: Element -> String -> Int -> String ->String
toCppHeader element elementParentName indentationAmount elementContent =
    let
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
