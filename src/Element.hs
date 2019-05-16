module Element(processNodes, toCpp) where

import           Data.ByteString.UTF8 as BSU
import           Data.List            as List
-- import           Debug.Trace          (trace)
import           App                  (toCppFooter, toCppHeader)
-- import           Common               (attributeValue)
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


elementsByParentId :: Int -> Model -> [ Element ]
elementsByParentId elementId model =
    List.filter
        (\element ->
            case Types.parent element of
                Just justParent ->
                    if justParent == elementId then
                        True
                    else
                        False
                Nothing ->
                    False
        )
        (document model)


toCpp' :: [ Element ] -> String -> Model -> Int -> String
toCpp' elements code model indentationAmount =
    if List.length elements == 0 then
        code
    else
        let
            element =
                List.head elements

            restOfElements =
                List.tail elements

            children =
                elementsByParentId (Types.id element) model
        in
        case Types.name element of
            "app" ->
                let
                    appCodeHeader =
                        App.toCppHeader element indentationAmount

                    appFooter =
                        Types.Element
                            { Types.name = "app_footer"
                            , Types.id = 0
                            , Types.parent = Types.parent element
                            , Types.content = Types.content element
                            , Types.attributes = Types.attributes element
                            }
                in
                toCpp'
                    (children ++ [ appFooter ] ++ restOfElements)
                    (code ++ appCodeHeader)
                    model
                    (indentationAmount + 1)

            "app_footer" ->
                let
                    appCodeFooter =
                        App.toCppFooter element indentationAmount
                in
                toCpp'
                    restOfElements
                    (code ++ appCodeFooter)
                    model
                    indentationAmount

            _ ->
                toCpp'
                    restOfElements
                    code
                    model
                    indentationAmount



toCpp :: Model -> String
toCpp model =
    if (List.length $ document model) /= 0 then
        let
            firstElement =
                List.head $ document model
        in
        toCpp' [ firstElement ] "" model 0
    else
        ""
    -- let
    --     elementsStringified =
    --         List.map
    --             (\element ->
    --                 show element
    --             )
    --             (document model)

    --     elementsStringifiedFolded =
    --         List.foldl
    --             (++)
    --             ""
    --             elementsStringified
    -- in
    -- elementsStringifiedFolded
