module File(writeMainSourceFile, writeMakeFile, writeEventsSourceFile, writePortSourceFile, writeTypesFile) where

import           Common                 (indent, indentation)
import           Data.List              as List
import           Data.Text              as Text
import           System.Directory.Paths (fileExists)
import           Types                  (Model (..))


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
writeMainSourceFile filepath _content =
    writeFile filepath _content


writeMakeFile :: FilePath -> IO ()
writeMakeFile filepath =
    let
        _content =
            "main: main.cpp events.cpp port.cpp\n"
                ++ "\tg++ `wx-config --libs --cxxflags` *.cpp -o main"
    in
    (fileExists (Text.pack filepath))
        >>= (\fe ->
                if fe == False then
                    (writeFile filepath _content)
                        >> (putStrLn "Makefile created.")
                else
                    return ()
            )


writeEventsSourceFile :: FilePath -> Model -> IO ()
writeEventsSourceFile filepath model =
    let
        oneIndentation =
            indentation 1

        _events =
            List.foldl
                (++)
                ""
                (List.map
                    (\e ->
                        "void Events::"
                            ++ (snd e)
                            ++ "(wxEvent& event) {\n"
                            ++ oneIndentation
                            ++ "int binding_index = GetBinding(\""
                            ++ (fst e)
                            ++ "\");\n\n"
                            ++ oneIndentation
                            ++ "if (binding_index == -1)\n"
                            ++ (indentation 2)
                            ++ "cout << \""
                            ++ (fst e)
                            ++ "\" << endl;\n"
                            ++ oneIndentation
                            ++ "else\n"
                            ++ (indentation 2)
                            ++ "this->bindings[binding_index]->function(event, Events::GetInstance()->GetApp());\n"
                            ++ "}\n\n"
                    )
                    (Types.events model)
                )

        _content =
            "#include \"types.h\"\n\n"
                ++ "Events* Events::instance;\n\n"
                ++ "Events* Events::GetInstance() {\n"
                ++ oneIndentation
                ++ "if (Events::instance == nullptr)\n"
                ++ oneIndentation
                ++ oneIndentation
                ++ "Events::instance = new Events();\n\n"
                ++ oneIndentation
                ++ "return Events::instance;\n"
                ++ "}\n\n"
                ++ "void Events::SetApp("
                ++ (Types.appName model)
                ++ "* app) {\n"
                ++ oneIndentation
                ++ "this->app = app;\n"
                ++ "}"
                ++ "\n\n"
                ++ (Types.appName model)
                ++ "* Events::GetApp() {\n"
                ++ oneIndentation
                ++ "return this->app;\n"
                ++ "}"
                ++ "\n\n"
                ++ "void Events::Bind(string name, func_t function) {\n"
                ++ oneIndentation
                ++ "Binding* binding = new Binding;\n"
                ++ oneIndentation
                ++ "binding->name = name;\n"
                ++ oneIndentation
                ++ "binding->function = function;\n\n"
                ++ oneIndentation
                ++ "this->bindings.push_back(binding);\n"
                ++ "}\n\n"
                ++ "int Events::GetBinding(string name) {\n"
                ++ oneIndentation
                ++ "for (int i = 0; i < this->bindings.size(); i++) {\n"
                ++ (indentation 2)
                ++ "if (this->bindings[i]->name.compare(name) == 0) {\n"
                ++ (indentation 3)
                ++ "return i;\n"
                ++ (indentation 2)
                ++ "}\n"
                ++ oneIndentation
                ++ "}\n\n"
                ++ oneIndentation
                ++ "return -1;\n"
                ++ "}\n\n"
                ++ _events

    in
    writeFile filepath _content


writePortSourceFile :: FilePath -> Model -> IO ()
writePortSourceFile filepath model =
    let
        oneIndentation =
            indentation 1

        _content =
            "#include \"types.h\"\n\n"
                ++ "void port("
                ++ (Types.appName model)
                ++ "* app) {\n"
                ++ oneIndentation
                ++ "\n"
                ++ "}\n"

    in
    (fileExists (Text.pack filepath))
        >>= (\fe ->
                if fe == False then
                    (writeFile filepath _content)
                        >> (putStrLn "port.cpp created.")
                else
                    return ()
            )


writeTypesFile :: FilePath -> Model -> IO ()
writeTypesFile filepath model =
    let
        declarations =
            List.foldl
                (\base ->
                    (\declaration ->
                        base
                            ++ (indentation 1)
                            ++ declaration
                            ++ "\n"
                    )
                )
                ""
                (Types.headerDeclarations model)

        _events =
            List.foldl
                (++)
                ""
                (List.map
                    (\e ->
                        (indentation 1)
                            ++ "void "
                            ++ (snd e)
                            ++ "(wxEvent&);\n"
                    )
                    (Types.events model)
                )

        _appName =
            Types.appName model

        _content =
            "#ifndef TYPES_H\n\n"
                ++ "#define TYPES_H\n\n"
                ++ "#include <iostream>\n"
                ++ "#include <vector>\n"
                ++ "#include <string>\n"
                ++ (processIncludes model)
                ++ "\n"
                ++ "using std::string;\n"
                ++ "using std::vector;\n"
                ++ "using std::cout;\n"
                ++ "using std::endl;\n\n"
                ++ "class "
                ++ _appName
                ++ " : public wxApp\n"
                ++ "{\n"
                ++ "public:\n"
                ++ (indent 1 "virtual bool OnInit();\n\n")
                ++ declarations
                ++ "};\n\n"
                ++ "typedef void (*func_t)(wxEvent&, "
                ++ _appName
                ++ "*);\n\n"
                ++ "struct Binding {\n"
                ++ (indent 1 "string name;\n")
                ++ (indent 1 "func_t function;\n")
                ++ "};\n\n"
                ++ "class Events {\n"
                ++ (indent 1 "vector<Binding*> bindings;\n")
                ++ (indent 1 _appName ++ "* app;\n\n")
                ++ "public:\n"
                ++ (indent 1 "static Events* instance;\n")
                ++ (indent 1 "static Events* GetInstance();\n\n")
                ++ (indent 1 "void SetApp(" ++ _appName ++ "*);\n")
                ++ (indent 1 _appName ++ "* GetApp();\n")
                ++ (indent 1 "void Bind(string, func_t);\n")
                ++ (indent 1 "int GetBinding(string);\n\n")
                ++ _events
                ++ "};\n\n"
                ++ "void port(" ++ _appName ++ "*);\n\n"
                ++ "#endif\n"

    in
    writeFile filepath _content
