module Common(attributeValue, indentation) where

import           Data.List as List
import           Types     (Element (..))

attributeValue :: String -> Element -> Maybe String
attributeValue attr element =
    let
        attrs =
            Types.attributes element

        searchResult =
            List.find
                (\item ->
                    if fst item == attr then
                        True
                    else
                        False
                )
                attrs
    in
    case searchResult of
        Just justSearchResult ->
            Just $ snd justSearchResult
        Nothing ->
            Nothing


indentation :: Int -> String
indentation amount =
    let
        indentationList =
            replicate amount "    "
    in
    List.foldl
        (++)
        ""
        indentationList
