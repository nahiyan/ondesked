module Types(Element(..), Model(..)) where

data Element =
    Element
        { name       :: String
        , id         :: Int
        , parent     :: Maybe Int
        , content    :: Maybe String
        , attributes :: [ (String, String) ]
        } deriving(Show)

data Model =
    Model
        { document           :: [ Element ]
        , parents            :: [ Int ]
        , includes           :: [ String ]
        , appName            :: String
        , events             :: [ ( String, String ) ]
        , headerDeclarations :: [ String ]
        }
