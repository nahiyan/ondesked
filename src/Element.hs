module Element(processNode, toCpp) where

import           Data.ByteString.UTF8 as BSU
import           Data.List            as List
import           Types                (Element (..), Model (..))
import           Xeno.DOM             (Node, name)

processNode :: Node -> Model -> Model
processNode node model =
    let
        element =
            Element
                (BSU.toString $ Xeno.DOM.name node)
                (case highestId $ Types.document model of
                    Just justHighestId ->
                        justHighestId
                    Nothing ->
                        1
                )
                (Just 0)
                (Just "Hey")
                []
    in
    model


highestId' :: [ Element ] -> Int -> Maybe Int
highestId' elements highest =
    if List.length elements == 0 then
        Just highest
    else
        let
            newHighest =
                if Types.id (head elements) > highest then
                    Types.id (head elements)
                else
                    highest
        in
        highestId' (tail elements) newHighest


highestId :: [ Element ] -> Maybe Int
highestId elements =
    if List.length elements == 0 then
        Nothing
    else
        highestId' elements (Types.id (head elements))


toCpp :: Model -> String
toCpp model =
    "Hello World!"
