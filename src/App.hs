module App(toCppHeader, toCppFooter) where

import           Common (attributeValue, indentation)
import           Types  (Element (..))

toCppHeader :: Element -> Int -> String
toCppHeader element indentationAmount =
    let
        id =
            case attributeValue "id" element of
                Just justId ->
                    justId
                Nothing ->
                    "MyApp"
    in
    (indentation indentationAmount) ++ "bool MyApp::OnInit()\n{\n"

toCppFooter :: Element -> Int -> String
toCppFooter element indentationAmount =
    (indentation indentationAmount) ++ "return true;\n}"
