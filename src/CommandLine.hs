module CommandLine(handle) where

import           Data.ByteString.UTF8 as BSU
import           Data.List            as List
import           Element              (processNodes, toCpp)
import           File                 (writeEventsSourceFile,
                                       writeMainSourceFile, writeMakeFile,
                                       writePortSourceFile, writeTypesFile)
-- import           System.Process       (readProcess)
import           System.Directory     (createDirectoryIfMissing)
import           System.FilePath      (pathSeparator, takeBaseName)
import           System.IO
import           Types                (Model (..))
import           Xeno.DOM             as XD


version :: String
version =
    "0.1.0a"

about :: String
about =
    "Ondesked is a markup language for developing GUI of desktop apps. You're using version " ++ version ++ ".\n"


help :: String
help =
    about
        ++ "\n"
        ++ "You can compile a document using:\n\n"
        ++ "\tondesked <file_path>\n\n"
        ++ "For example, if you want to compile a document named \"program.xml\":\n\n"
        ++ "\tondesked program.xml\n"
        ++ "\t"


handle :: [ String ] -> IO ()
handle args =
    if List.length args == 0 then
        putStrLn help
    else
        case head args of
            "--version" ->
                putStrLn $ show version

            "-v" ->
                putStrLn $ show version

            "--help" ->
                putStrLn help

            "-h" ->
                putStrLn help

            a ->
                handleFile a


handleFile :: String -> IO ()
handleFile filepath =
    (openFile filepath ReadMode)
        >>= hGetContents
        >>= (\byteString ->
                let
                    parsed = XD.parse $ BSU.fromString byteString
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

                            dirName =
                                takeBaseName filepath

                            prefix =
                                dirName ++ [ pathSeparator ]
                        in
                        (createDirectoryIfMissing True dirName)
                            >> (writeMainSourceFile (prefix ++ "main.cpp") source)
                            >> (writeMakeFile (prefix ++ "Makefile"))
                            >> (writeEventsSourceFile (prefix ++ "events.cpp") newModel)
                            >> (writePortSourceFile (prefix ++ "port.cpp") newModel)
                            >> (writeTypesFile (prefix ++ "types.h") newModel)
                            -- >> readProcess "make" ["-C", "app"] ""
                            -- >>= (\_ ->
                            --         putStrLn "Built app successfully!"
                            --     )
                            -- >> putStrLn "Running app..."
                            -- >> readProcess "./app/main" [] ""
                            -- >>= putStrLn


                    Left exception ->
                        putStrLn $ show exception
            )
