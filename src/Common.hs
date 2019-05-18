module Common(attributeValue, indentation, elementNameFromId, elementName, hasClass, elementFromId, addEvent, hasAttribute, eventMethodName) where

import           Data.List       as List
import           Data.List.Split (splitOn)
import           Text.Casing     (fromAny, toPascal)
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
                _elementId =
                    Types.id justElement

                attributeId =
                    attributeValue "id" justElement
            in
            case attributeId of
                Just justAttributeId ->
                    Just justAttributeId

                Nothing ->
                    Just ((Types.name justElement) ++ (show _elementId))
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


hasAttribute :: String -> Element -> Bool
hasAttribute attribute element =
    case attributeValue attribute element of
        Just _ ->
            True
        Nothing ->
            False


addEvent :: String -> String -> Model -> Model
addEvent _name eName model =
    let
        newEvents =
            (Types.events model)
                ++ [ ( _name, eName ) ]
    in
    Model
        { document = Types.document model
        , parents  = Types.parents model
        , includes = Types.includes model
        , appName  = Types.appName model
        , events   = newEvents
        }


eventMethodName :: Element -> String -> String
eventMethodName element suffix =
    toPascal
        (fromAny
            (
                (elementName element) ++ suffix
            )
        )
