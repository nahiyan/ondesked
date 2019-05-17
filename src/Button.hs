module Button(toCppHeader) where

import           Common      (attributeValue, elementName, indentation)
import           Text.Casing (fromAny, toPascal)
import           Types       (Element (..), Model (..))


toCppHeader :: Element -> String -> Int -> String -> Model -> ( String, Model )
toCppHeader element elementParentName indentationAmount elementContent model =
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

        maybeOnclickAttribute =
            attributeValue "onclick" element

        _events =
            case maybeOnclickAttribute of
                Just _ ->
                    prefix
                        ++ eName
                        ++ "->Bind(wxEVT_BUTTON, &Events::"
                        ++ (toPascal (fromAny eName))
                        ++ "Click, "
                        ++ "Events::GetInstance());\n"
                Nothing ->
                    ""
        string =
            instantiation
                ++ _events
                ++ "\n"

        newEvents =
            Types.events model
                ++
                    (case maybeOnclickAttribute of
                        Just justOnClickAttribute ->
                            (Types.events model)
                                ++ [ ( justOnClickAttribute, (toPascal (fromAny eName)) ++ "Click" ) ]
                        Nothing ->
                            []
                    )


        newModel =
            Model
                { document = Types.document model
                , parents  = Types.parents model
                , includes = Types.includes model
                , appName  = Types.appName model
                , events   = newEvents
                }
    in
    ( string, newModel )
