module TableItem(toCppHeader) where

import           Common (addHeaderDeclaration, elementName, indentation)
import           Types  (Element (..), Model (..))


toCppHeader :: Element -> String -> Int -> String -> Model -> ( String, Model )
toCppHeader element nameOfGreatGrandParent indentationAmount elementContent model =
    let
        eName =
            elementName element

        prefix =
            indentation indentationAmount

        instantiation =
            prefix
                ++ nameOfGreatGrandParent
                ++ "->SetItem(" ++ nameOfGreatGrandParent ++ "_y, " ++ nameOfGreatGrandParent ++ "_x, \""
                ++ elementContent
                ++ "\");\n"
                ++ prefix
                ++ nameOfGreatGrandParent ++ "_x = (" ++ nameOfGreatGrandParent ++ "_x + 1) % " ++ nameOfGreatGrandParent ++ "_cols;\n"

        string =
            instantiation
                ++ "\n"

        newModel =
            addHeaderDeclaration
                ("wxListItem* " ++ eName ++ ";")
                model

    in
    ( string, newModel )
