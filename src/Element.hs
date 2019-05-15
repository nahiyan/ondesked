module Element(processNodes, toCpp) where

import           Data.ByteString.UTF8 as BSU
import           Data.List            as List
-- import           Debug.Trace          (trace)
import           Types                (Element (..), Model (..))
import           Xeno.DOM             (Content (Element, Text), Node,
                                       attributes, contents, name)


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

            parentOfElement =
                if (List.length $ parents model) /= 0 then
                    Just (List.head $ parents model)
                else
                    Nothing

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

            elementId =
                case highestId $ document model of
                    Just justHighestId ->
                        justHighestId + 1
                    Nothing ->
                        1

            element =
                Types.Element
                    (BSU.toString $ Xeno.DOM.name node)
                    elementId
                    parentOfElement
                    elementContent
                    (stringifyAttributes $ Xeno.DOM.attributes node)

            newDocument =
                (document model) ++ [ element ]

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

            parentAdditions =
                (List.map
                    (\_ ->
                        elementId
                    )
                    elementsNodified
                )

            newParents =
                if (List.length $ parents model) == 0 then
                    parentAdditions
                else
                    (List.tail $ parents model)
                        ++
                            parentAdditions

            newModel =
                Model
                    { document = newDocument
                    , parents = newParents
                    }
        in
        processNodes restOfNodes newModel


highestId' :: [ Element ] -> Int -> Maybe Int
highestId' elements highest =
    if List.length elements == 0 then
        Just highest
    else
        let
            currentElement =
                head elements

            newHighest =
                if Types.id currentElement > highest then
                    Types.id currentElement
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
