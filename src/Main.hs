module Main where

import           Data.ByteString.UTF8 as BSU
import           Element              (processNodes, toCpp)
import           File                 (writeEventsSourceFile,
                                       writeMainSourceFile, writeMakeFile,
                                       writePortSourceFile, writeTypesFile)
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
                                , headerDeclarations = []
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
                        >> (writeMakeFile "app/Makefile")
                        >> (writeEventsSourceFile "app/events.cpp" newModel)
                        >> (writePortSourceFile "app/port.cpp" newModel)
                        >> (writeTypesFile "app/types.h" newModel)
                        -- >> readProcess "make" ["-C", "app"] ""
                        -- >>= (\_ ->
                        --         putStrLn "Built app successfully!"
                        --     )
                        -- >> putStrLn "Running app..."
                        -- >> readProcess "./app/main" [] ""
                        -- >>= putStrLn
                    )

                Left exception ->
                    putStrLn $ show exception
        )
