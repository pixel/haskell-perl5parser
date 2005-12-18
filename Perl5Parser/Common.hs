module Perl5Parser.Common
    ( show4debug
    , map_t2
    , internal_error
    , isNothing, fromMaybe
    ) where

import Data.Maybe (fromMaybe, isNothing)
import System.IO.Unsafe (unsafePerformIO)

map_t2 f (a,b) = (f a, f b)

show4debug :: Show a => String -> a -> a
show4debug s e = seq (unsafePerformIO $ putStrLn (s ++ ": " ++ show e)) e

internal_error s = error ("internal error: " ++ s)
