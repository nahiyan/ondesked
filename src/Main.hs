module Main where

import           Data.ByteString.UTF8 as BSU
import           Element              (processNode, toCpp)
import           System.IO
import           Types                (Model (..))
import           Xeno.DOM             (parse)

initialModel :: Model
initialModel =
    Model
        { document = []
        , level = 0
        , nesting = []
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
                    putStrLn $ toCpp (processNode node initialModel)

                Left exception ->
                    putStrLn $ show exception
        )
