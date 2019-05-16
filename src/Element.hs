module Element(processNodes, toCpp, elementById) where

import           Data.ByteString.UTF8 as BSU
import           Data.List            as List
-- import           Debug.Trace          (trace)
import           App                  (toCppFooter, toCppHeader)
import           BoxSizer             (toCppFooter, toCppHeader)
import           Button               (toCppHeader)
import           Frame                (toCppHeader)
import           Panel                (toCppHeader)
import           Textarea             (toCppHeader)
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


elementById :: Int -> Model -> Maybe Element
elementById elementId model =
    let
        filterResult =
            List.filter
                (\element ->
                    if elementId == Types.id element then
                        True
                    else
                        False
                )
                (document model)
    in
    if List.length filterResult == 0 then
        Nothing
    else
        Just (List.head filterResult)


parentByElement :: Element -> Model -> Maybe Element
parentByElement element model =
    let
        maybeParentId =
            Types.parent element
    in
    case maybeParentId of
        Just justParentId ->
            case elementById justParentId model of
                Just justParentElement ->
                    if "box_sizer" == Types.name justParentElement then
                        let
                            maybeGrandParentElement =
                                case Types.parent justParentElement of
                                    Just grandParentId ->
                                        elementById grandParentId model
                                    Nothing ->
                                        Nothing
                        in
                        maybeGrandParentElement
                    else
                        Just justParentElement
                Nothing ->
                    Nothing

        Nothing ->
            Nothing


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
                        App.toCppFooter indentationAmount
                in
                toCpp'
                    restOfElements
                    (code ++ appCodeFooter)
                    model
                    indentationAmount

            "frame" ->
                let
                    elementParent =
                        case Types.parent element of
                            Just justParentId ->
                                let
                                    e = elementById justParentId model
                                in
                                case e of
                                    Just justE ->
                                        if "app" == Types.name justE then
                                            Nothing
                                        else
                                            e

                                    Nothing ->
                                        Nothing

                            Nothing ->
                                Nothing

                    frameCodeHeader =
                        Frame.toCppHeader
                            element
                            elementParent
                            indentationAmount
                in
                toCpp'
                    (children ++ restOfElements)
                    (code ++ frameCodeHeader)
                    model
                    indentationAmount

            "box_sizer" ->
                let
                    elementParent =
                        case Types.parent element of
                            Just justParentId ->
                                elementById justParentId model

                            Nothing ->
                                Nothing

                    boxSizerCodeHeader =
                        BoxSizer.toCppHeader
                            element
                            elementParent
                            indentationAmount

                    boxSizerFooter =
                        Types.Element
                            { Types.name = "box_sizer_footer"
                            , Types.id = Types.id element
                            , Types.parent = Types.parent element
                            , Types.content = Nothing
                            , Types.attributes = Types.attributes element
                            }
                in
                toCpp'
                    (children ++ [ boxSizerFooter ] ++ restOfElements)
                    (code ++ boxSizerCodeHeader)
                    model
                    indentationAmount

            "box_sizer_footer" ->
                let
                    elementParent =
                        case Types.parent element of
                            Just justParentId ->
                                elementById justParentId model

                            Nothing ->
                                Nothing

                    boxSizerCodeFooter =
                        BoxSizer.toCppFooter
                            element
                            elementParent
                            children
                            indentationAmount
                in
                toCpp'
                    restOfElements
                    (code ++ boxSizerCodeFooter)
                    model
                    indentationAmount

            "panel" ->
                let
                    elementParent =
                        parentByElement element model

                    panelCodeHeader =
                        Panel.toCppHeader
                            element
                            elementParent
                            indentationAmount
                in
                toCpp'
                    (children ++ restOfElements)
                    (code ++ panelCodeHeader)
                    model
                    indentationAmount

            "button" ->
                let
                    elementParent =
                        parentByElement element model

                    elementContent =
                        case Types.content element of
                            Just justElementContent ->
                                justElementContent
                            Nothing ->
                                "Default"

                    buttonCodeHeader =
                        Button.toCppHeader
                            element
                            elementParent
                            indentationAmount
                            elementContent
                in
                toCpp'
                    restOfElements
                    (code ++ buttonCodeHeader)
                    model
                    indentationAmount

            "textarea" ->
                let
                    elementParent =
                        parentByElement element model

                    elementContent =
                        case Types.content element of
                            Just justElementContent ->
                                justElementContent
                            Nothing ->
                                "Default"

                    textareaCodeHeader =
                        Textarea.toCppHeader
                            element
                            elementParent
                            indentationAmount
                            elementContent
                in
                toCpp'
                    restOfElements
                    (code ++ textareaCodeHeader)
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
