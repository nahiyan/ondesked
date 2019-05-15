module Element(processNodes, toCpp) where

import           Data.ByteString.UTF8 as BSU
import           Data.List            as List
-- import           Debug.Trace          (trace)
import           Types                (Element (..), Model (..))
import           Xeno.DOM             (Content (Text, Element), Node, attributes,
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

            texts =
                List.filter
                    (\contentItem ->
                        case contentItem of
                            Text _ ->
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

            elementContent =
                if List.length textsFolded == 0 then
                    Nothing
                else
                    Just textsFolded

            element =
                Types.Element
                    (BSU.toString $ Xeno.DOM.name node)
                    (case highestId $ document model of
                        Just justHighestId ->
                            justHighestId
                        Nothing ->
                            1
                    )
                    Nothing
                    elementContent
                    (stringifyAttributes $ Xeno.DOM.attributes node)

            newDocument =
                (document model) ++ [ element ]

            newModel =
                Model
                    { document = newDocument
                    , level = level model
                    , nesting = nesting model
                    }

            elements =
                List.filter
                    (\contentItem ->
                        case contentItem of
                            Xeno.DOM.Element _ ->
                                True
                            _ ->
                                False
                    )
                    (contents node)

            elementsNodified =
                List.map
                    (\(Xeno.DOM.Element elementNode) ->
                        elementNode
                    )
                    elements

            restOfNodes =
                (List.tail nodes) ++ elementsNodified
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
    let
        elementsStringified =
            List.map
                (\element ->
                    show element
                )
                (document model)

        elementsStringifiedFolded =
            List.foldl
                (++)
                ""
                elementsStringified
    in
    elementsStringifiedFolded