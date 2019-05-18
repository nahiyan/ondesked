module MenuBar(toCppHeader, toCppFooter) where

import           Common (addHeaderDeclaration, elementFromId, elementName,
                         indentation)
import           Types  (Element (..), Model (..))


toCppHeader :: Element -> Int -> Model -> ( String, Model )
toCppHeader element indentationAmount model =
    let
        eName =
            elementName element

        prefix =
            indentation indentationAmount

        newModel =
            addHeaderDeclaration
                ("wxMenuBar* " ++ eName ++ ";")
                model

        _code =
            prefix
                ++ eName
                ++ " = new wxMenuBar();"
                ++ "\n\n"
    in
    ( _code, newModel )

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
