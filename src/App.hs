module App(toCppHeader, toCppFooter) where

import           Common (elementName, indentation)
import           Types  (Element (..), Model (..))


toCppHeader :: Element -> Int -> Model -> ( String, Model )
toCppHeader element indentationAmount model =
    let
        aName =
            elementName element

        newModel =
            model
                { appName = aName}

        string =
            (indentation indentationAmount)
                ++ "#include \"types.h\"\n\n"
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
        ++ "Events::GetInstance()->SetApp(this);\n\n"
        ++ (indentation indentationAmount)
        ++ "port(this);\n\n"
        ++ (indentation indentationAmount)
        ++ "return true;\n}\n"
