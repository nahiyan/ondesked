module MenuItem(toCppHeader) where

import           Common (indentation)


toCppHeader :: String -> Int -> String-> String
toCppHeader elementParentName indentationAmount elementContent =
    let
        prefix =
            indentation indentationAmount
    in
    prefix
        ++ elementParentName
        ++ "->Append(wxID_NEW, wxT(\""
        ++ elementContent
        ++ "\"));\n"
