module Element(processNodes, toCpp, elementById) where

import           Data.ByteString.UTF8 as BSU
import           Data.List            as List
-- import           Debug.Trace          (trace)
import           App                  (toCppFooter, toCppHeader)
import           BoxSizer             (toCppFooter, toCppHeader)
import           Button               (toCppHeader)
import           Common               (elementName)
import           Data.String.HT       (trim)
import           Frame                (toCppHeader)
import           Menu                 (toCppFooter, toCppHeader)
import           MenuBar              (toCppFooter, toCppHeader)
import           MenuItem             (toCppHeader)
import           Panel                (toCppHeader)
import           Textarea             (toCppHeader)
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
                Model
                    { document = newDocument
                    , parents = newParents
                    , includes = Types.includes model
                    , appName = Types.appName model
                    , events = Types.events model
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
        in
        case Types.name element of
            "app" ->
                let
                    appCodeHeader =
                        App.toCppHeader
                            element
                            indentationAmount
                            model

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
                    (code ++ (fst appCodeHeader))
                    (snd appCodeHeader)
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
                    frameCodeHeader =
                        Frame.toCppHeader
                            element
                            nameOfParent
                            indentationAmount
                in
                toCpp'
                    (children ++ restOfElements)
                    (code ++ frameCodeHeader)
                    model
                    indentationAmount

            "box_sizer" ->
                let
                    boxSizerCodeHeader =
                        BoxSizer.toCppHeader
                            element
                            nameOfParent
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
                    boxSizerCodeFooter =
                        BoxSizer.toCppFooter
                            element
                            nameOfParent
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
                    panelCodeHeader =
                        Panel.toCppHeader
                            element
                            nameOfParent
                            indentationAmount
                in
                toCpp'
                    (children ++ restOfElements)
                    (code ++ panelCodeHeader)
                    model
                    indentationAmount

            "button" ->
                let
                    elementContent =
                        case Types.content element of
                            Just justElementContent ->
                                justElementContent
                            Nothing ->
                                "Default"

                    buttonCodeHeader =
                        Button.toCppHeader
                            element
                            nameOfParent
                            indentationAmount
                            elementContent
                            model
                in
                toCpp'
                    restOfElements
                    (code ++ (fst buttonCodeHeader))
                    (snd buttonCodeHeader)
                    indentationAmount

            "textarea" ->
                let
                    elementContent =
                        case Types.content element of
                            Just justElementContent ->
                                justElementContent
                            Nothing ->
                                "Default"

                    textareaCodeHeader =
                        Textarea.toCppHeader
                            element
                            nameOfParent
                            indentationAmount
                            elementContent
                in
                toCpp'
                    restOfElements
                    (code ++ textareaCodeHeader)
                    model
                    indentationAmount

            "menu_bar" ->
                let
                    codeAddition =
                        MenuBar.toCppHeader
                            element
                            indentationAmount

                    footer =
                        Types.Element
                            { Types.name = "menu_bar_footer"
                            , Types.id = Types.id element
                            , Types.parent = Types.parent element
                            , Types.content = Nothing
                            , Types.attributes = Types.attributes element
                            }
                in
                toCpp'
                    (children ++ [ footer ] ++ restOfElements)
                    (code ++ codeAddition)
                    model
                    indentationAmount

            "menu_bar_footer" ->
                let
                    codeAddition =
                        MenuBar.toCppFooter
                            element
                            nameOfParent
                            indentationAmount
                            model
                in
                toCpp'
                    restOfElements
                    (code ++ codeAddition)
                    model
                    indentationAmount

            "menu" ->
                let
                    codeAddition =
                        Menu.toCppHeader
                            element
                            indentationAmount

                    footer =
                        Types.Element
                            { Types.name = "menu_footer"
                            , Types.id = Types.id element
                            , Types.parent = Types.parent element
                            , Types.content = Nothing
                            , Types.attributes = Types.attributes element
                            }
                in
                toCpp'
                    (children ++ [ footer ] ++ restOfElements)
                    (code ++ codeAddition)
                    model
                    indentationAmount

            "menu_footer" ->
                let
                    codeAddition =
                        Menu.toCppFooter
                            element
                            nameOfParent
                            indentationAmount
                            model
                in
                toCpp'
                    restOfElements
                    (code ++ codeAddition)
                    model
                    indentationAmount

            "menu_item" ->
                let
                    elementContent =
                        case Types.content element of
                            Just justElementContent ->
                                justElementContent
                            Nothing ->
                                "Default"

                    codeAddition =
                        MenuItem.toCppHeader
                            element
                            nameOfParent
                            indentationAmount
                            elementContent
                            model
                in
                toCpp'
                    restOfElements
                    (code ++ (fst codeAddition))
                    (snd codeAddition)
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
