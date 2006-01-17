module Perl5Parser.Common
    ( join_
    , map_t2, map_non_empty_list
    , show4debug
    , internal_error
    , isNothing, fromMaybe
    ) where

import List (intersperse)
import Data.Maybe (fromMaybe, isNothing)
import System.IO.Unsafe (unsafePerformIO)

join_ :: [a] -> [[a]] -> [a]
join_ s = concat . intersperse s

map_t2 f (a,b) = (f a, f b)

map_non_empty_list :: ([a] -> b) -> [a] -> [b]
map_non_empty_list _ [] = []
map_non_empty_list f l = [f l]

show4debug :: Show a => String -> a -> a
show4debug s e = seq (unsafePerformIO $ putStrLn (s ++ ": " ++ show e)) e

internal_error s = error ("internal error: " ++ s)
