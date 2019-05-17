module App(toCppHeader, toCppFooter) where

import           Common (elementName, indentation)
import           Types  (Element (..), Model (..))


toCppHeader :: Element -> Int -> Model -> ( String, Model )
toCppHeader element indentationAmount model =
    let
        aName =
            elementName element

        newModel =
            Model
                { appName = aName
                , document = Types.document model
                , parents = Types.parents model
                , includes = Types.includes model
                }

        string =
            (indentation indentationAmount)
                ++ "#include \"main.h\"\n\n"
                ++ "IMPLEMENT_APP("
                ++ aName
                ++ ")\n\n"
                ++ "bool "
                ++ aName
                ++ "::OnInit()\n{\n"
    in
    ( string, newModel )


toCppFooter :: Int -> String
toCppFooter indentationAmount =
    (indentation indentationAmount)
        ++ "return true;\n}"
