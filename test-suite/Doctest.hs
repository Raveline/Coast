-- test-suite/DocTest.hs
module Main (main) where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = glob "src/**/*.hs" >>= handling -- doctest

handling :: [String] -> IO ()
handling x = do print x
                doctest x
