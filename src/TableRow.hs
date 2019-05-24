module TableRow(toCppHeader, toCppFooter) where

import           Common (addHeaderDeclaration, elementFromId, elementName,
                         indentation)
import           Types  (Element (..), Model (..))



toCppHeader :: Element -> String -> Int -> Model -> ( String, Model )
toCppHeader element nameOfGrandParent indentationAmount model =
    let
        eName =
            elementName element

        prefix =
            indentation indentationAmount

        parentName =
            case Types.parent element of
                Just parentId ->
                    case elementFromId parentId model of
                        Just _parent ->
                            Types.name _parent

                        Nothing ->
                            ""

                Nothing ->
                    ""

        instantiation =
            prefix
                ++ eName
                ++ " = new wxListItem();\n"
                ++ prefix
                ++ eName
                ++ "->SetId("
                ++ nameOfGrandParent
                ++ "_y);\n"
                ++ prefix
                ++ eName
                ++ "->SetWidth(wxLIST_AUTOSIZE_USEHEADER);\n"
                ++ prefix
                ++ nameOfGrandParent
                ++ "->InsertItem(*"
                ++ eName
                ++ ");\n"

        string =
            if "tbody" == parentName then
                instantiation
                    ++ "\n"
            else
                ""

        newModel =
            addHeaderDeclaration
                ("wxListItem* " ++ eName ++ ";")
                model

    in
    ( string, newModel )


toCppFooter :: Element -> String -> Int -> Model -> ( String, Model )
toCppFooter element nameOfParent indentationAmount model =
    let
        prefix =
            indentation indentationAmount

        parentName =
            case Types.parent element of
                Just parentId ->
                    case elementFromId parentId model of
                        Just _parent ->
                            Types.name _parent

                        Nothing ->
                            ""

                Nothing ->
                    ""

        instantiation =
            if "tbody" == parentName then
                prefix
                    ++ nameOfParent
                    ++ "_y++;"
                    ++ "\n"

            else
                ""

        string =
            instantiation
                ++ "\n"

    in
    ( string, model )
