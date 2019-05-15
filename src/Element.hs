module Element(processNodes, toCpp) where

import           Data.ByteString.UTF8 as BSU
import           Data.List            as List
import           Debug.Trace          (trace)
import           Types                (Element (..), Model (..))
import           Xeno.DOM             (Content (Text), Node, attributes,
                                       contents, name)


stringifyAttributes :: [ (ByteString, ByteString) ] -> [ (String, String) ]
stringifyAttributes byteStringAttributes =
    List.map
        (\attribute ->
            let
                first = BSU.toString $ fst attribute
                second = BSU.toString $ snd attribute
            in
            (first, second)
        )
        byteStringAttributes


processNodes :: [ Node ] -> Model -> Model
processNodes nodes model =
    if List.length nodes == 0 then
        model
    else
        let
            node =
                List.head nodes

            restOfNodes =
                List.tail nodes

            newDocument =
                (document model) ++ [ element ]

            newModel =
                Model
                    { document = newDocument
                    , level = level model
                    , nesting = nesting model
                    }

            texts =
                List.filter
                    (\contentItem ->
                        case contentItem of
                            Text a ->
                                True
                            _ ->
                                False
                    )
                    (contents node)

            textsStringified =
                List.map
                    (\(Text text) ->
                        BSU.toString text
                    )
                    texts

            textsFolded =
                List.foldl
                    (\base->
                        (\contentItem2 ->
                            base ++ contentItem2
                        )
                    )
                    ""
                    textsStringified

            content =
                if List.length textsFolded == 0 then
                    Nothing
                else
                    Just textsFolded

            element =
                trace "Element" (Element
                    (BSU.toString $ Xeno.DOM.name node)
                    (case highestId $ Types.document model of
                        Just justHighestId ->
                            justHighestId
                        Nothing ->
                            1
                    )
                    (Just 0)
                    content
                    (stringifyAttributes $ Xeno.DOM.attributes node))
        in
        processNodes restOfNodes newModel


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
