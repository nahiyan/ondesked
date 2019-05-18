module Menu(toCppHeader, toCppFooter) where

import           Common (addHeaderDeclaration, attributeValue, elementFromId,
                         elementName, indentation)
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
                ("wxMenu* " ++ eName ++ ";")
                model

        _code =
            prefix
                ++ eName
                ++ " = new wxMenu();"
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

        headerElementAttributeName =
            case elementFromId (Types.id element) model of
                Just justHeaderElement ->
                    case attributeValue "name" justHeaderElement of
                        Just justAttributeName ->
                            justAttributeName
                        Nothing ->
                            ""
                Nothing ->
                    ""

        headerElementParentType =
            case elementFromId (Types.id element) model of
                Just justHeaderElement ->
                    case Types.parent justHeaderElement of
                        Just justHeaderElementParentId ->
                            case elementFromId justHeaderElementParentId model of
                                Just justElementFromId ->
                                    Types.name justElementFromId
                                Nothing ->
                                    ""
                        Nothing ->
                            ""
                Nothing ->
                    ""

        prefix =
            indentation indentationAmount
    in
    if headerElementParentType == "menu_bar" then
        prefix
            ++ elementParentName
            ++ "->Append("
            ++ headerElementName
            ++ ", wxT(\""
            ++ headerElementAttributeName
            ++ "\")"
            ++ ");"
            ++ "\n\n"
    else
        prefix
            ++ elementParentName
            ++ "->Append(wxID_NEW, "
            ++ "wxT(\""
            ++ headerElementAttributeName
            ++ "\"), "
            ++ headerElementName
            ++ ");"
            ++ "\n\n"
