module Main where

import           Data.ByteString.UTF8 as BSU
import           Element              (processNodes, toCpp)
import           File                 (writeEventHandlersHeaderFile,
                                       writeEventHandlersSourceFile,
                                       writeEventsHeaderFile,
                                       writeEventsSourceFile,
                                       writeMainHeaderFile, writeMainSourceFile,
                                       writeMakeFile)
import           System.IO
-- import           System.Process       (readProcess)
import           Types                (Model (..))
import           Xeno.DOM             (parse)


main :: IO ()
main =
    (openFile "data/program.xml" ReadMode)
    >>= hGetContents
    >>= (\bs ->
            let
                parsed = parse $ BSU.fromString bs
            in
            case parsed of
                Right node ->
                    let
                        initialModel =
                            Model
                                { document = []
                                , parents = []
                                , includes = [ "<wx/wx.h>" ]
                                , appName = "Default App"
                                , events = []
                                }

                        processedNodes =
                            processNodes
                                [ node ]
                                initialModel

                        cpp =
                            toCpp processedNodes

                        source =
                            fst cpp

                        newModel =
                            snd cpp
                    in
                    ((writeMainSourceFile "app/main.cpp" source)
                        >> (writeMainHeaderFile "app/main.h" newModel)
                        >> (writeMakeFile "app/Makefile")
                        >> (writeEventsSourceFile "app/events.cpp" newModel)
                        >> (writeEventsHeaderFile "app/events.h" newModel)
                        >> (writeEventHandlersSourceFile "app/event_handlers.cpp")
                        >> (writeEventHandlersHeaderFile "app/event_handlers.h")
                        -- >> readProcess "make" ["-C", "app"] ""
                        -- >>= (\_ ->
                        --         putStrLn "Built app successfully!"
                        --     )
                        -- >> readProcess "./app/main" [] ""
                        -- >>= (\_ ->
                        --         putStrLn "Running app..."
                        --     )
                    )

                Left exception ->
                    putStrLn $ show exception
        )
