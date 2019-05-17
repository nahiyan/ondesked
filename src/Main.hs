module Main where

import           Data.ByteString.UTF8 as BSU
import           Element              (processNodes, toCpp)
import           File                 (writeMainHeaderFile, writeMainSourceFile,
                                       writeMakeFile)
import           System.IO
import           System.Process       (readProcess)
import           Types                (Model (..))
import           Xeno.DOM             (parse)

initialModel :: Model
initialModel =
    Model
        { document = []
        , parents = []
        , includes = [ "<wx/wx.h>" ]
        , appName = "Default App"
        }

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
                        processedNodes =
                            processNodes
                                [ node ]
                                initialModel

                        cpp =
                            toCpp processedNodes
                    in
                    ((writeMainSourceFile "app/main.cpp" (fst cpp))
                        >> (writeMainHeaderFile "app/main.h" (snd cpp))
                        >> (writeMakeFile "app/Makefile")
                        >> readProcess "make" ["-C", "app"] ""
                        >>= (\_ ->
                                putStrLn "Built app successfully!"
                            )
                        >> readProcess "./app/main" [] ""
                        >>= (\_ ->
                                putStrLn "Running app..."
                            )
                    )

                Left exception ->
                    putStrLn $ show exception
        )
