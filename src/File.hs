module File(writeMainSourceFile, writeMainHeaderFile, writeMakeFile, writeEventsSourceFile, writeEventsHeaderFile, writeEventHandlersSourceFile, writeEventHandlersHeaderFile) where

import           Common                 (indentation)
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


writeMainHeaderFile :: FilePath -> Model -> IO ()
writeMainHeaderFile filepath model =
    let
        _content =
            "#ifndef MAIN_H\n\n"
                ++ "#define MAIN_H\n\n"
                ++ "#include \"events.h\"\n"
                ++ "#include \"event_handlers.h\"\n"
                ++ processIncludes model
                ++ "\n"
                ++ "class "
                ++ (Types.appName model)
                ++ " : public wxApp\n"
                ++ "{\n"
                ++ "public:\n"
                ++ (indentation 1)
                ++ "virtual bool OnInit();"
                ++ "\n"
                ++ "};\n\n"
                ++ "#endif"
                ++ "\n"
    in
    writeFile filepath _content


writeMakeFile :: FilePath -> IO ()
writeMakeFile filepath =
    let
        _content =
            "main: main.cpp events.cpp event_handlers.cpp\n"
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
                            ++ "this->bindings[binding_index]->function(event);\n"
                            ++ "}\n\n"
                    )
                    (Types.events model)
                )

        _content =
            "#include \"events.h\"\n\n"
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


writeEventsHeaderFile :: FilePath -> Model -> IO ()
writeEventsHeaderFile filepath model =
    let
        oneIndentation =
            indentation 1

        _events =
            List.foldl
                (++)
                ""
                (List.map
                    (\e ->
                        oneIndentation
                            ++ "void "
                            ++ (snd e)
                            ++ "(wxEvent&);\n"
                    )
                    (Types.events model)
                )

        _content =
            "#ifndef EVENTS_H\n\n"
                ++ "#define EVENTS_H\n\n"
                ++ "#include <iostream>\n"
                ++ "#include <vector>\n"
                ++ "#include <string>\n"
                ++ "#include <wx/wx.h>\n\n"

                ++ "using std::string;\n"
                ++ "using std::vector;\n"
                ++ "using std::cout;\n"
                ++ "using std::endl;\n\n"

                ++ "typedef void (*func_t)(wxEvent&);\n\n"

                ++ "struct Binding {\n"
                ++ oneIndentation
                ++ "string name;\n"
                ++ oneIndentation
                ++ "func_t function;\n"
                ++ "};\n\n"

                ++ "class Events {\n"
                ++ oneIndentation
                ++ "vector<Binding*> bindings;\n\n"
                ++ "public:\n"
                ++ oneIndentation
                ++ "static Events* instance;\n"
                ++ oneIndentation
                ++ "static Events* GetInstance();\n\n"
                ++ oneIndentation
                ++ "void Bind(string, func_t);\n"
                ++ oneIndentation
                ++ "int GetBinding(string);\n\n"
                ++ _events
                ++ "};\n\n"
                ++ "#endif"
                ++ "\n"

    in
    writeFile filepath _content


writeEventHandlersSourceFile :: FilePath -> IO ()
writeEventHandlersSourceFile filepath =
    let
        oneIndentation =
            indentation 1

        _content =
            "#include \"event_handlers.h\"\n\n"
                ++ "void bind_handlers() {\n"
                ++ oneIndentation
                ++ "\n"
                ++ "}\n"

    in
    (fileExists (Text.pack filepath))
        >>= (\fe ->
                if fe == False then
                    (writeFile filepath _content)
                        >> (putStrLn "event_handlers.cpp created.")
                else
                    return ()
            )


writeEventHandlersHeaderFile :: FilePath -> IO ()
writeEventHandlersHeaderFile filepath =
    let
        _content =
            "#ifndef HANDLE_EVENTS_H\n\n"
                ++ "#define HANDLE_EVENTS_H\n\n"
                ++ "#include \"events.h\"\n\n"
                ++ "void bind_handlers();\n\n"
                ++ "#endif\n"

    in
    (fileExists (Text.pack filepath))
        >>= (\fe ->
                if fe == False then
                    (writeFile filepath _content)
                        >> (putStrLn "event_handlers.h created.")
                else
                    return ()
            )

