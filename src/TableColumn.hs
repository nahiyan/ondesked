module TableColumn(toCppHeader) where

import           Common (indentation)
import           Types  (Model (..))


toCppHeader :: String -> Int -> String -> Model -> ( String, Model )
toCppHeader nameOfGreatGrandParent indentationAmount elementContent model =
    let
        prefix =
            indentation indentationAmount

        instantiation =
            prefix
                ++ nameOfGreatGrandParent
                ++ "->AppendColumn(\""
                ++ elementContent
                ++ "\");"
                ++ "\n"
                ++ prefix
                ++ nameOfGreatGrandParent
                ++ "_cols++;\n"

        string =
            instantiation
                ++ "\n"

    in
    ( string, model )
