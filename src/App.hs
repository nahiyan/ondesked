module App(toCppHeader, toCppFooter) where

import           Common (elementName, indentation)
import           Types  (Element (..))


toCppHeader :: Element -> Int -> String
toCppHeader element indentationAmount =
    (indentation indentationAmount)
        ++ "bool "
        ++ (elementName element)
        ++ "::OnInit()\n{\n"


toCppFooter :: Int -> String
toCppFooter indentationAmount =
    (indentation indentationAmount) ++ "return true;\n}"
