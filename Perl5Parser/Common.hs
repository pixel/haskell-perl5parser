module Perl5Parser.Common
    ( show4debug
    , map_t2, map_non_empty_list
    , internal_error
    , isNothing, fromMaybe
    ) where

import Data.Maybe (fromMaybe, isNothing)
import System.IO.Unsafe (unsafePerformIO)

map_t2 f (a,b) = (f a, f b)

map_non_empty_list :: ([a] -> b) -> [a] -> [b]
map_non_empty_list _ [] = []
map_non_empty_list f l = [f l]

show4debug :: Show a => String -> a -> a
show4debug s e = seq (unsafePerformIO $ putStrLn (s ++ ": " ++ show e)) e

internal_error s = error ("internal error: " ++ s)
