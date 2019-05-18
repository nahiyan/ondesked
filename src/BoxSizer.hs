module BoxSizer(toCppHeader, toCppFooter) where

import           Common          (addHeaderDeclaration, attributeValue,
                                  elementName, hasClass, indentation)
import           Data.List       as List
import           Data.List.Split (splitOn)
import           Types           (Element (..), Model (..))


toCppHeader :: Element -> String -> Int -> Model -> ( String, Model )
toCppHeader element elementParentName indentationAmount model =
    let
        eName =
            elementName element

        prefix =
            indentation indentationAmount

        orientation =
            if True == hasClass "horizontal" element then
                "wxHORIZONTAL"
            else
                "wxVERTICAL"

        instantiation =
            prefix
                ++ eName
                ++ " = new wxBoxSizer("
                ++ orientation ++ ");\n\n"

        setSizer =
            prefix
                ++ elementParentName
                ++ "->SetSizer("
                ++ eName
                ++ ");\n"

        _code =
            instantiation
                ++ setSizer
                ++ "\n"

        newModel =
            addHeaderDeclaration
                ("wxBoxSizer* " ++ eName ++ ";")
                model
    in
    ( _code, newModel )


flagsFromDirections' :: String -> String -> String
flagsFromDirections' directions flags =
    if List.length directions /= 0 then
        let
            direction =
                List.head directions

            newFlags =
                flags
                    ++ (case direction of
                        'l' ->
                            " | wxLEFT"

                        'r' ->
                            " | wxRIGHT"

                        't' ->
                            " | wxTOP"

                        'b' ->
                            " | wxBOTTOM"

                        'x' ->
                            " | wxLEFT | wxRIGHT"

                        'y' ->
                            " | wxTOP | wxBOTTOM"

                        _ ->
                            ""
                    )
        in
        flagsFromDirections' (List.tail directions) newFlags
    else
        flags


flagsFromDirections :: String -> String
flagsFromDirections directions =
    flagsFromDirections' directions ""


toCppFooter :: Element -> String -> [ Element ] -> Int -> String
toCppFooter element elementParentName children indentationAmount =
    let
        eName =
            elementName element

        prefix =
            indentation indentationAmount

        additions =
            List.foldl
                (++)
                ""
                (List.map
                    (\child ->
                        let
                            grow =
                                case attributeValue "grow" child of
                                    Just justGrow ->
                                        justGrow
                                    Nothing ->
                                        "0"

                            padding =
                                case attributeValue "padding" child of
                                    Just justPadding ->
                                        justPadding
                                    Nothing ->
                                        "xy-0"

                            paddingSplit =
                                splitOn "-" padding

                            flags =
                                "wxEXPAND"
                                    ++
                                    (if List.length paddingSplit == 2 then
                                        flagsFromDirections $ List.head paddingSplit
                                    else
                                        ""
                                    )

                            paddingValue =
                                (if List.length paddingSplit == 2 then
                                    (List.head (List.drop 1 paddingSplit))
                                else
                                    ""
                                )
                        in
                        prefix
                            ++ eName
                            ++ "->Add("
                            ++ (elementName child)
                            ++ ", "
                            ++ grow
                            ++ ", "
                            ++ flags
                            ++ ", "
                            ++ paddingValue
                            ++ ");\n"
                    )
                    children)

        fitParent =
            if True == hasClass "fit-parent" element then
                prefix
                    ++ elementParentName
                    ++ "->GetSizer()->Fit("
                    ++ elementParentName
                    ++ ");\n"
            else
                ""
    in
    additions
        ++ fitParent
        ++ "\n"
