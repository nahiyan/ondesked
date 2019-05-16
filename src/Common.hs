module Common(attributeValue, indentation, elementNameFromId, elementName, hasClass) where

import           Data.List       as List
import           Data.List.Split (splitOn)
import           Types           (Element (..), Model (..))

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


elementFromId :: Int -> Model -> Maybe Element
elementFromId elementId model =
    let
        searchResult =
            List.find
                (\element ->
                    if (Types.id element) == elementId then
                        True
                    else
                        False
                )
                (document model)
    in
    searchResult


elementNameFromId :: Int -> Model -> Maybe String
elementNameFromId elementId model =
    let
        searchResult =
            List.find
                (\element ->
                    if (Types.id element) == elementId then
                        True
                    else
                        False
                )
                (document model)
    in
    case searchResult of
        Just justElement ->
            let
                elementId =
                    Types.id justElement

                attributeId =
                    attributeValue "id" justElement
            in
            case attributeId of
                Just justAttributeId ->
                    Just justAttributeId

                Nothing ->
                    Just ((Types.name justElement) ++ (show elementId))
        Nothing ->
            Nothing

elementName :: Element -> String
elementName element =
    let
        elementId =
            Types.id element

        attributeId =
            attributeValue "id" element
    in
    case attributeId of
        Just justAttributeId ->
            justAttributeId

        Nothing ->
            (Types.name element) ++ (show elementId)


hasClass :: String -> Element -> Bool
hasClass _class element =
    let
        classes =
            case attributeValue "class" element of
                Just justClasses ->
                    splitOn " " justClasses
                Nothing ->
                    []
        searchResult =
            List.find
                (\searchItem ->
                    if searchItem == _class then
                        True
                    else
                        False
                )
                classes
    in
    if List.length searchResult /= 0 then
        True
    else
        False
