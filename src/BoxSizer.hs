module BoxSizer(toCppHeader, toCppFooter) where

import           Common    (attributeValue, elementName, hasClass, indentation)
import           Data.List as List
import           Types     (Element (..))

toCppHeader :: Element -> Maybe Element -> Int -> String
toCppHeader element elementParent indentationAmount =
    let
        elementParentName =
            case elementParent of
                Just justElementParent ->
                    elementName justElementParent
                Nothing ->
                    "NULL"

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
                ++ "wxBoxSizer* "
                ++ eName
                ++ " = new wxBoxSizer("
                ++ orientation ++ ");\n\n"

        setSizer =
            prefix
                ++ elementParentName
                ++ "->SetSizer("
                ++ eName
                ++ ");\n"
    in
    instantiation
        ++ setSizer

toCppFooter :: Element -> Maybe Element -> [ Element ] -> Int -> String
toCppFooter element elementParent children indentationAmount =
    let
        elementParentName =
            case elementParent of
                Just justElementParent ->
                    elementName justElementParent
                Nothing ->
                    "NULL"

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
                                        "1"

                            flags =
                                "wxEXPAND"

                            padding =
                                "0"
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
                            ++ padding
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
