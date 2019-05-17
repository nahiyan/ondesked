module File(writeMainSourceFile, writeMainHeaderFile, writeMakeFile) where

import           Common    (indentation)
import           Data.List as List
import           Types     (Model (..))


processIncludes :: Model -> String
processIncludes model =
    let
        includesCodified =
            List.map
                (\include ->
                    "#include " ++ include ++ "\n"
                )
                (includes model)
    in
    List.foldl
        (++)
        ""
        includesCodified


writeMainSourceFile :: FilePath -> String -> IO ()
writeMainSourceFile filepath content =
    writeFile filepath content


writeMainHeaderFile :: FilePath -> Model -> IO ()
writeMainHeaderFile filepath model =
    let
        content =
            processIncludes model
            ++ "\n"
            ++ "class "
            ++ (Types.appName model)
            ++ " : public wxApp\n"
            ++ "{\n"
            ++ "public:\n"
            ++ (indentation 1)
            ++ "virtual bool OnInit();"
            ++ "\n"
            ++ "};"
    in
    writeFile filepath content


writeMakeFile :: FilePath -> IO ()
writeMakeFile filepath =
    let
        content =
            "main: main.cpp\n"
                ++ "\tg++ `wx-config --libs --cxxflags` *.cpp -o main"
    in
    writeFile filepath content
