module Types(Element(..), Model(..)) where

data Element =
    Element
        { name       :: String
        , id         :: Int
        , parent     :: Maybe Int
        , content    :: Maybe String
        , attributes :: [ (String, String) ]
        } deriving (Show)

data Model =
    Model
        { document :: [ Element ]
        , level    :: Int
        , nesting  :: [ ( Int, Element ) ]
        }
