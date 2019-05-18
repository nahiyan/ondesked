module Button(toCppHeader) where

import           Common (addEvent, addHeaderDeclaration, attributeValue,
                         elementName, eventMethodName, indentation)
import           Types  (Element (..), Model (..))


toCppHeader :: Element -> String -> Int -> String -> Model -> ( String, Model )
toCppHeader element elementParentName indentationAmount elementContent model =
    let
        eName =
            elementName element

        prefix =
            indentation indentationAmount

        instantiation =
            prefix
                ++ eName
                ++ " = new wxButton("
                ++ elementParentName
                ++ ", wxID_ANY, wxT(\""
                ++ elementContent
                ++ "\"));"
                ++ "\n"

        maybeOnclickAttribute =
            attributeValue "onclick" element

        _eventMethodName =
            eventMethodName element "Click"

        _events =
            case maybeOnclickAttribute of
                Just _ ->
                    prefix
                        ++ eName
                        ++ "->Bind(wxEVT_BUTTON, &Events::"
                        ++ _eventMethodName
                        ++ ", "
                        ++ "Events::GetInstance());\n"
                Nothing ->
                    ""
        string =
            instantiation
                ++ _events
                ++ "\n"

        newModel =
            case maybeOnclickAttribute of
                Just justOnclick ->
                    addEvent justOnclick _eventMethodName model
                Nothing ->
                    model

        newModel2 =
            addHeaderDeclaration
                ("wxButton* " ++ eName ++ ";")
                newModel
    in
    ( string, newModel2 )
