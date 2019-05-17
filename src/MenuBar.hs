module MenuBar(toCppHeader, toCppFooter) where

import           Common (elementFromId, elementName, indentation)
import           Types  (Element (..), Model (..))


toCppHeader :: Element -> Int -> String
toCppHeader element indentationAmount =
    let
        eName =
            elementName element

        prefix =
            indentation indentationAmount
    in
    prefix
        ++ "wxMenuBar* "
        ++ eName
        ++ " = new wxMenuBar();"
        ++ "\n\n"

toCppFooter :: Element -> String -> Int -> Model -> String
toCppFooter element elementParentName indentationAmount model =
    let
        headerElementName =
            case elementFromId (Types.id element) model of
                Just justHeaderElement ->
                    elementName justHeaderElement
                Nothing ->
                    "NULL"

        prefix =
            indentation indentationAmount
    in
    prefix
        ++ elementParentName
        ++ "->SetMenuBar("
        ++ headerElementName
        ++ ");\n"
