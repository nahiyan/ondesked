module Main where

import           CommandLine        as CL
import           System.Environment (getArgs)


main :: IO ()
main =
    getArgs
        >>= (\args ->
                CL.handle args
            )
