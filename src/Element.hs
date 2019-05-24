module Element(processNodes, toCpp, elementById, greatGrandParentNameByElement) where

import           App                  (toCppFooter, toCppHeader)
import           BoxSizer             (toCppFooter, toCppHeader)
import           Button               (toCppHeader)
import           CollapsiblePane      (toCppHeader)
import           CollapsiblePanePane  (toCppHeader)
import           Common               (elementName)
import           Data.ByteString.UTF8 as BSU
import           Data.List            as List
import           Data.String.HT       (trim)
-- import           Debug.Trace          (trace)
import           Frame                (toCppHeader)
import           Menu                 (toCppFooter, toCppHeader)
import           MenuBar              (toCppFooter, toCppHeader)
import           MenuItem             (toCppHeader)
import           Panel                (toCppHeader)
import           Table                (toCppHeader)
import           TableColumn          (toCppHeader)
import           TableItem            (toCppHeader)
import           TableRow             (toCppFooter, toCppHeader)
import           Text                 (toCppHeader)
import           Textarea             (toCppHeader)
import           Textfield            (toCppHeader)
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
                trim
                    (List.foldl
                        (\base->
                            (\contentItem2 ->
                                base ++ contentItem2
                            )
                        )
                        ""
                        textsStringified
                    )

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
                model
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


parentNameByElement :: Element -> Model -> String
parentNameByElement element model =
    let
        maybeParent = parentByElement element model
    in
    case maybeParent of
        Just justParent ->
            if "app" == Types.name justParent then
                "NULL"
            else
                elementName justParent

        Nothing ->
            "NULL"


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


greatGrandParentNameByElement :: Element -> Model -> String
greatGrandParentNameByElement element model =
    case parentByElement element model of
        Just _parent ->
            case parentByElement _parent model of
                Just _grandParent ->
                    case parentByElement _grandParent model of
                        Just _greatGrandParent ->
                            elementName _greatGrandParent

                        Nothing ->
                            "NULL"

                Nothing ->
                    "NULL"

        Nothing ->
            "NULL"


grandParentNameByElement :: Element -> Model -> String
grandParentNameByElement element model =
    case parentByElement element model of
        Just _parent ->
            case parentByElement _parent model of
                Just _grandParent ->
                    elementName _grandParent

                Nothing ->
                    "NULL"

        Nothing ->
            "NULL"


toCpp' :: [ Element ] -> String -> Model -> Int -> ( String, Model)
toCpp' elements code model indentationAmount =
    if List.length elements == 0 then
        ( code, model )
    else
        let
            element =
                List.head elements

            restOfElements =
                List.tail elements

            children =
                elementsByParentId (Types.id element) model

            nameOfParent =
                parentNameByElement element model

            elementContent =
                case Types.content element of
                    Just justElementContent ->
                        justElementContent
                    Nothing ->
                        "Default"

            nameOfGreatGrandParent =
                greatGrandParentNameByElement element model

            nameOfGrandParent =
                grandParentNameByElement element model
        in
        case Types.name element of
            "app" ->
                let
                    codeAndModel =
                        App.toCppHeader
                            element
                            indentationAmount
                            model

                    footer =
                        element
                            { Types.name = "app_footer"
                            , Types.id = 0
                            }
                in
                toCpp'
                    (children ++ [ footer ] ++ restOfElements)
                    (code ++ (fst codeAndModel))
                    (snd codeAndModel)
                    (indentationAmount + 1)

            "app_footer" ->
                let
                    _code =
                        App.toCppFooter indentationAmount
                in
                toCpp'
                    restOfElements
                    (code ++ _code)
                    model
                    indentationAmount

            "frame" ->
                let
                    codeAndModel =
                        Frame.toCppHeader
                            element
                            nameOfParent
                            indentationAmount
                            model
                in
                toCpp'
                    (children ++ restOfElements)
                    (code ++ (fst codeAndModel))
                    (snd codeAndModel)
                    indentationAmount

            "box_sizer" ->
                let
                    codeAndModel =
                        BoxSizer.toCppHeader
                            element
                            nameOfParent
                            indentationAmount
                            model

                    footer =
                        element
                            { Types.name = "box_sizer_footer"
                            , Types.content = Nothing
                            }
                in
                toCpp'
                    (children ++ [ footer ] ++ restOfElements)
                    (code ++ (fst codeAndModel))
                    (snd codeAndModel)
                    indentationAmount

            "box_sizer_footer" ->
                let
                    _code =
                        BoxSizer.toCppFooter
                            element
                            nameOfParent
                            children
                            indentationAmount
                            model
                in
                toCpp'
                    restOfElements
                    (code ++ _code)
                    model
                    indentationAmount

            "panel" ->
                let
                    codeAndModel =
                        Panel.toCppHeader
                            element
                            nameOfParent
                            indentationAmount
                            model
                in
                toCpp'
                    (children ++ restOfElements)
                    (code ++ (fst codeAndModel))
                    (snd codeAndModel)
                    indentationAmount

            "table" ->
                let
                    codeAndModel =
                        Table.toCppHeader
                            element
                            nameOfParent
                            indentationAmount
                            model
                in
                toCpp'
                    (children ++ restOfElements)
                    (code ++ (fst codeAndModel))
                    (snd codeAndModel)
                    indentationAmount

            "thead" ->
                toCpp'
                    (children ++ restOfElements)
                    code
                    model
                    indentationAmount

            "tr" ->
                let
                    codeAndModel =
                        TableRow.toCppHeader
                            element
                            nameOfGrandParent
                            indentationAmount
                            model

                    footer =
                        element
                            { Types.name = "tr_footer" }
                in
                toCpp'
                    (children ++ [ footer ] ++ restOfElements)
                    (code ++ (fst codeAndModel))
                    (snd codeAndModel)
                    indentationAmount

            "tr_footer" ->
                let
                    codeAndModel =
                        TableRow.toCppFooter
                            element
                            nameOfGrandParent
                            indentationAmount
                            model
                in
                toCpp'
                    (restOfElements)
                    (code ++ (fst codeAndModel))
                    (snd codeAndModel)
                    indentationAmount

            "tbody" ->
                toCpp'
                    (children ++ restOfElements)
                    code
                    model
                    indentationAmount

            "th" ->
                let
                    codeAndModel =
                        TableColumn.toCppHeader
                            nameOfGreatGrandParent
                            indentationAmount
                            elementContent
                            model
                in
                toCpp'
                    restOfElements
                    (code ++ (fst codeAndModel))
                    (snd codeAndModel)
                    indentationAmount

            "td" ->
                let
                    codeAndModel =
                        TableItem.toCppHeader
                            element
                            nameOfGreatGrandParent
                            indentationAmount
                            elementContent
                            model
                in
                toCpp'
                    restOfElements
                    (code ++ (fst codeAndModel))
                    (snd codeAndModel)
                    indentationAmount

            "button" ->
                let
                    codeAndModel =
                        Button.toCppHeader
                            element
                            nameOfParent
                            indentationAmount
                            elementContent
                            model
                in
                toCpp'
                    restOfElements
                    (code ++ (fst codeAndModel))
                    (snd codeAndModel)
                    indentationAmount

            "textarea" ->
                let
                    codeAndModel =
                        Textarea.toCppHeader
                            element
                            nameOfParent
                            indentationAmount
                            elementContent
                            model
                in
                toCpp'
                    restOfElements
                    (code ++ (fst codeAndModel))
                    (snd codeAndModel)
                    indentationAmount

            "textfield" ->
                let
                    codeAndModel =
                        Textfield.toCppHeader
                            element
                            nameOfParent
                            indentationAmount
                            elementContent
                            model
                in
                toCpp'
                    restOfElements
                    (code ++ (fst codeAndModel))
                    (snd codeAndModel)
                    indentationAmount

            "menu_bar" ->
                let
                    codeAndModel =
                        MenuBar.toCppHeader
                            element
                            indentationAmount
                            model

                    footer =
                        element
                            { Types.name = "menu_bar_footer"
                            , Types.content = Nothing
                            }
                in
                toCpp'
                    (children ++ [ footer ] ++ restOfElements)
                    (code ++ (fst codeAndModel))
                    (snd codeAndModel)
                    indentationAmount

            "colpane" ->
                let
                    codeAndModel =
                        CollapsiblePane.toCppHeader
                            element
                            nameOfParent
                            indentationAmount
                            model

                    colPanePaneId =
                        case highestId $ Types.document model of
                            Just _highestId ->
                                _highestId + 1

                            Nothing ->
                                1

                    colPanePaneElement =
                        element
                            { Types.name = "colpanepane"
                            , Types.parent = Just $ Types.id element
                            , Types.id = colPanePaneId
                            }

                    _children =
                        [ colPanePaneElement
                        ]
                            ++ (List.map
                                   (\e ->
                                       e
                                           { Types.parent = Just colPanePaneId }
                                   )
                                   children
                               )

                    newModel =
                        (snd codeAndModel)
                            { Types.document = (Types.document (snd codeAndModel)) ++ [ colPanePaneElement ] }

                    newModel2 =
                        newModel
                            { Types.document =
                                List.map
                                    (\e ->
                                        case Types.parent e of
                                            Just _parent ->
                                                if _parent == Types.id element && Types.id e /= colPanePaneId then
                                                    e
                                                        { Types.parent = Just colPanePaneId }
                                                else
                                                    e

                                            Nothing ->
                                                e
                                    )
                                    $ Types.document newModel
                            }
                in
                toCpp'
                    (_children ++ restOfElements)
                    (code ++ (fst codeAndModel))
                    newModel2
                    indentationAmount

            "colpanepane" ->
                let
                    codeAndModel =
                        CollapsiblePanePane.toCppHeader
                            element
                            nameOfParent
                            indentationAmount
                            model
                in
                toCpp'
                    (restOfElements)
                    (code ++ (fst codeAndModel))
                    (snd codeAndModel)
                    indentationAmount

            "menu_bar_footer" ->
                let
                    _code =
                        MenuBar.toCppFooter
                            element
                            nameOfParent
                            indentationAmount
                            model
                in
                toCpp'
                    restOfElements
                    (code ++ _code)
                    model
                    indentationAmount

            "menu" ->
                let
                    codeAndModel =
                        Menu.toCppHeader
                            element
                            indentationAmount
                            model

                    footer =
                        element
                            { Types.name = "menu_footer"
                            , Types.content = Nothing
                            }
                in
                toCpp'
                    (children ++ [ footer ] ++ restOfElements)
                    (code ++ (fst codeAndModel))
                    (snd codeAndModel)
                    indentationAmount

            "menu_footer" ->
                let
                    _code =
                        Menu.toCppFooter
                            element
                            nameOfParent
                            indentationAmount
                            model
                in
                toCpp'
                    restOfElements
                    (code ++ _code)
                    model
                    indentationAmount

            "menu_item" ->
                let
                    codeAndModel =
                        MenuItem.toCppHeader
                            element
                            nameOfParent
                            indentationAmount
                            elementContent
                            model
                in
                toCpp'
                    restOfElements
                    (code ++ (fst codeAndModel))
                    (snd codeAndModel)
                    indentationAmount

            "text" ->
                let
                    codeAndModel =
                        Text.toCppHeader
                            element
                            nameOfParent
                            indentationAmount
                            elementContent
                            model
                in
                toCpp'
                    restOfElements
                    (code ++ (fst codeAndModel))
                    (snd codeAndModel)
                    indentationAmount

            _ ->
                toCpp'
                    restOfElements
                    code
                    model
                    indentationAmount



toCpp :: Model -> ( String, Model )
toCpp model =
    if (List.length $ document model) /= 0 then
        let
            firstElement =
                List.head $ document model
        in
        toCpp' [ firstElement ] "" model 0
    else
        ( "", model )
