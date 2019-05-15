module Main where

import           Data.ByteString.UTF8 as BSU
import           Element              (processNodes, toCpp)
import           System.IO
import           Types                (Model (..))
import           Xeno.DOM             (parse)

initialModel :: Model
initialModel =
    Model
        { document = []
        , parents = []
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
                    putStrLn $ toCpp (processNodes [ node ] initialModel)

                Left exception ->
                    putStrLn $ show exception
        )
