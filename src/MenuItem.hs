module MenuItem(toCppHeader) where

import           Common (addEvent, attributeValue, eventMethodName,
                         hasAttribute, indentation)

import           Types  (Element (..), Model (..))


toCppHeader :: Element -> String -> Int -> String-> Model -> ( String, Model )
toCppHeader element elementParentName indentationAmount elementContent model =
    let
        prefix =
            indentation indentationAmount

        maybeOnclick =
            attributeValue "onclick" element

        hasOnclick =
            hasAttribute "onclick" element

        menuItemId =
            if hasOnclick == True then
                show $ Types.id element
            else
                "wxID_NEW"

        _eventMethodName =
            eventMethodName element "Click"

        _events =
            if hasOnclick == True then
                prefix
                    ++ "Bind(wxEVT_MENU, &Events::"
                    ++ _eventMethodName
                    ++ ", Events::GetInstance(), "
                    ++ menuItemId
                    ++ ");"
                    ++ "\n\n"
            else
                ""

        newModel =
            case maybeOnclick of
                Just justOnclick ->
                    addEvent justOnclick _eventMethodName model
                Nothing ->
                    model

        _code =
            prefix
            ++ elementParentName
            ++ "->Append("
            ++ menuItemId
            ++ ", wxT(\""
            ++ elementContent
            ++ "\"));\n"
            ++ _events
    in
    ( _code, newModel )
